package org.moddingx.sourcetransform.parchment

import com.google.gson.{Gson, GsonBuilder}
import org.moddingx.sourcetransform.util.inheritance.InheritanceIO
import org.moddingx.sourcetransform.util.{Bytecode, LanguageLevel, SourceUtil, Util}
import joptsimple.util.{PathConverter, PathProperties}
import joptsimple.{OptionException, OptionParser}
import org.eclipse.jdt.core.dom.CompilationUnit
import org.parchmentmc.feather.io.gson.{MDCGsonAdapterFactory, NamedAdapter, OffsetDateTimeAdapter, SimpleVersionAdapter}
import org.parchmentmc.feather.mapping.{MappingDataBuilder, VersionedMDCDelegate, VersionedMappingDataContainer}
import org.parchmentmc.feather.named.Named
import org.parchmentmc.feather.util.SimpleVersion

import java.io.File
import java.nio.file.{Files, StandardOpenOption}
import java.time.OffsetDateTime
import java.util.concurrent.{Future, ScheduledThreadPoolExecutor}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.given

object ParchmentSanitizer {

  val GSON: Gson = {
    val builder = new GsonBuilder
    builder.disableHtmlEscaping
    builder.setPrettyPrinting()
    builder.registerTypeAdapter(classOf[SimpleVersion], new SimpleVersionAdapter)
    builder.registerTypeAdapter(classOf[OffsetDateTime], new OffsetDateTimeAdapter())
    builder.registerTypeAdapter(classOf[Named], new NamedAdapter())
    builder.registerTypeAdapterFactory(new MDCGsonAdapterFactory())
    builder.create
  }

  def run(args: String*): Unit = {
    val options = new OptionParser(false)
    val specSources = options.acceptsAll(List("s", "sources").asJava, "Folder with source files.").withRequiredArg().withValuesConvertedBy(new PathConverter(PathProperties.DIRECTORY_EXISTING))
    val specInheritance = options.acceptsAll(List("i", "inheritance").asJava, "The inheritance map to use").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specClasspath = options.acceptsAll(List("p", "classpath").asJava, "Library classpath. Must also include the jars / jmods from the java installation.").withRequiredArg().withValuesSeparatedBy(File.pathSeparator).withValuesConvertedBy(new PathConverter())
    val specLevel = options.acceptsAll(List("l", "level").asJava, "Source level").withRequiredArg().withValuesConvertedBy(Util.enumArg[LanguageLevel]).defaultsTo(LanguageLevel.DEFAULT)
    val specInput = options.acceptsAll(List("m", "input").asJava, "Input for the parchment export to process.").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specOutput = options.acceptsAll(List("o", "output").asJava, "Output for the sanitized parchment export.").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specQuiet = options.acceptsAll(List("q", "quiet").asJava, "Suppress warning message while reading source code.")
    val specIgnore = options.acceptsAll(List("b", "ignore").asJava, "Packages to ignore (with all subpackages).").withRequiredArg().withValuesSeparatedBy(',')
    val specSrg = options.acceptsAll(List("srg").asJava, "Run in RSG mode: Parameters matching the pattern p_\\d+_ are considered unique. All sanitizers for methods with the same SRG parameters are merged.")
    val set = try {
      options.parse(args: _*)
    } catch {
      case e: OptionException =>
        System.err.println("Option exception: " + e.getMessage)
        options.printHelpOn(System.err)
        Util.exit(0)
    }
    if (!set.has(specSources) || !set.has(specInheritance) || !set.has(specInput) || !set.has(specOutput)) {
      if (!set.has(specSources)) System.out.println("Missing required option: " + specSources)
      if (!set.has(specInheritance)) System.out.println("Missing required option: " + specInheritance)
      if (!set.has(specInput)) System.out.println("Missing required option: " + specInput)
      if (!set.has(specOutput)) System.out.println("Missing required option: " + specOutput)
      options.printHelpOn(System.out)
      System.exit(1)
    } else {
      val inheritanceReader = Files.newBufferedReader(set.valueOf(specInheritance))
      val inheritance = InheritanceIO.read(inheritanceReader)
      inheritanceReader.close()

      val ignored = set.valuesOf(specIgnore).asScala.map(_.replace('.', '/')).toSet
      val ignoredPkgs = ignored.map(_ + "/")

      val base = set.valueOf(specSources).toAbsolutePath.normalize()
      val sources = SourceUtil.getJavaSources(base).filter(f => !ignored.contains(f) && !ignoredPkgs.exists(f.startsWith))
      val createParser = SourceUtil.createParserFactory(set.valueOf(specLevel), base, set.valuesOf(specClasspath).asScala.toSeq) _

      val quiet = set.has(specQuiet)
      val srgMode = set.has(specSrg)

      val executor = new ScheduledThreadPoolExecutor(1 max (Runtime.getRuntime.availableProcessors() - 1), (action: Runnable) => {
        val thread = new Thread(action)
        thread.setDaemon(true)
        thread
      })
      val sourceFilesBetweenNotify = 1 max (100 min (sources.size / 20))
      var completed = 0
      val lock = new AnyRef
      val futures = ListBuffer[Future[_]]()

      val mapBuilder = mutable.Map[Bytecode.Method, ParamRenamer]()
      val lambdaBuilder = mutable.Map[SourceUtil.Lambda, LambdaTarget]()
      val srgBuilder = mutable.Map[String, mutable.Set[Bytecode.Method]]()

      for (file <- sources) {
        futures.addOne(executor.submit(new Runnable {
          override def run(): Unit = {
            val currentMap = mutable.Map[Bytecode.Method, ParamRenamer]()
            val lambdaMap = mutable.Map[SourceUtil.Lambda, LambdaTarget]()
            val srgMap = mutable.Map[String, mutable.Set[Bytecode.Method]]()
            try {
              val parser = createParser(file)
              parser.createAST(null) match {
                case cu: CompilationUnit =>
                  val visitor = new SourceVisitor(inheritance, currentMap, lambdaMap, srgMap, quiet)
                  cu.accept(visitor)
                  visitor.endParse()
                case x => System.err.println("Parser returned invalid result for " + file + ": " + x.getClass + " " + x)
              }
            } catch {
              case e: Exception => throw new IllegalStateException("Failed to process class: " + file, e)
            }
            lock.synchronized {
              mapBuilder.addAll(currentMap)
              lambdaBuilder.addAll(lambdaMap)
              srgMap.foreach(entry => srgBuilder.getOrElseUpdate(entry._1, mutable.Set()).addAll(entry._2))
              completed += 1
              if (completed % sourceFilesBetweenNotify == 0) {
                val percentage = Math.round(100 * (completed / sources.size.toDouble))
                if (percentage < 100) {
                  System.err.println(s"$percentage% ($completed / ${sources.size})")
                }
              }
            }
          }
        }))
      }
      futures.foreach(_.get())
      executor.shutdownNow()
      System.err.println(s"100% (${sources.size} / ${sources.size})")

      val map: Map[Bytecode.Method, ParamRenamer] = mapBuilder.toMap
      val lambdas: Map[SourceUtil.Lambda, LambdaTarget] = lambdaBuilder.toMap

      val srgs: Map[String, Set[Bytecode.Method]] = if (srgMode) srgBuilder.map(entry => (entry._1, entry._2.toSet)).toMap else Map()
      val reverseSrgs: Map[Bytecode.Method, Set[String]] = srgs.flatMap(entry => entry._2.map(m => (entry._1, m))).groupMap(_._2)(_._1).map(entry => (entry._1, entry._2.toSet))

      def getRenamer(method: Bytecode.Method): ParamRenamer = {
        if (!srgMode) {
          map.getOrElse(method, ParamRenamer.Fallback)
        } else {
          val allMethods: Set[Bytecode.Method] = reverseSrgs.getOrElse(method, Set()).flatMap(srg => srgs.getOrElse(srg, Set()))
          ParamRenamer.merge(map.getOrElse(method, ParamRenamer.Fallback), allMethods.flatMap(method => map.get(method)))
        }
      }

      val inputReader = Files.newBufferedReader(set.valueOf(specInput))
      val input = GSON.fromJson(inputReader, classOf[VersionedMappingDataContainer])
      inputReader.close()

      val output = new MappingDataBuilder

      for (pkg <- input.getPackages.asScala) output.createPackage(pkg.getName).addJavadoc(pkg.getJavadoc)

      for (cls <- input.getClasses.asScala) {
        val classBuilder = output.createClass(cls.getName).addJavadoc(cls.getJavadoc)
        cls.getFields.forEach(fd => classBuilder.createField(fd.getName, fd.getDescriptor).addJavadoc(fd.getJavadoc))
        
        // New renames that include sanitized parameters
        // Required, so lambda parameters don't conflict with method parameters
        val renamedMappers = mutable.Map[Bytecode.Method, ParamRenamer]()
        
        for (md <- cls.getMethods.asScala) {
          val method = Bytecode.Method(cls.getName, md.getName, md.getDescriptor)
          // First process non-lambdas, lambdas need the results of non-lambda methods
          if (!method.name.startsWith("lambda$")) {
            val renamer: ParamRenamer = getRenamer(method)
            val renamedParameters = mutable.Set[String]()
            val methodBuilder = classBuilder.createMethod(md.getName, md.getDescriptor).addJavadoc(md.getJavadoc)
            
            for (param <- md.getParameters.asScala) {
              val newName = renamer.rename(param.getName)
              methodBuilder.createParameter(param.getIndex).setName(newName).setJavadoc(param.getJavadoc)
              renamedParameters.addOne(newName)
            }
            
            renamedMappers.put(method, renamer.withExcludedNames(renamedParameters.toSet))
          }
        }

        for (md <- cls.getMethods.asScala) {
          val method = Bytecode.Method(cls.getName, md.getName, md.getDescriptor)
          // Now to the lambdas
          if (method.name.startsWith("lambda$")) {
            val sourceLambdas: Set[SourceUtil.Lambda] = inheritance.getImplementedLambdas(method).map(SourceUtil.wrapLambda)
            // Only process known lambdas where all source code occurrences have been found.
            if (sourceLambdas.nonEmpty && sourceLambdas.forall(lambdas.contains)) {
              val lambdaTargets = sourceLambdas.flatMap(lambdas.get)
              def resolveLambda(lambdaMethod: Bytecode.Method): ParamRenamer = renamedMappers.getOrElse(lambdaMethod, getRenamer(lambdaMethod))
              
              val mapperOptions: Seq[Option[ParamRenamer]] = lambdaTargets.toSeq.map(t => t.resolve(resolveLambda))
              
              // Check that the implementation method matches at least one lambda usage
              // If the same lambda method is used twice, it'll get the name of the first occurrence
              // So in case of some compiler weirdness, we have an additional safety point with this.
              if (!lambdaTargets.exists(target => target.canMatch(md.getName))) {
                println("Skipping lambda as bytecode and sourcecode name mismatch: For lambda:" + md.getName + ", targets: " + lambdaTargets.flatMap(_.lambdaName()).mkString("(", ",", ")"))
              } else if (mapperOptions.forall(_.isDefined)) {
                // If one renamer is not defined, it was set to skip
                // in that case, the entire lambda needs to be skipped
                val mappers = mapperOptions.map(_.get)
                // A fallback mapper indicates no data about the lambda, needs to be skipped
                if (!mappers.contains(ParamRenamer.Fallback)) {
                  val mergedMapper = ParamRenamer.merge(ParamRenamer.Keep, mappers.toSet)
                  val methodBuilder = classBuilder.createMethod(md.getName, md.getDescriptor).addJavadoc(md.getJavadoc)
                  for (param <- md.getParameters.asScala) {
                    methodBuilder.createParameter(param.getIndex).setName(mergedMapper.rename(param.getName)).setJavadoc(param.getJavadoc)
                  }
                }
              }
            } else {
              if (!set.has(specQuiet)) {
                if (sourceLambdas.isEmpty) {
                  println("Skipping lambda-like method as it has no known use as a lambda: " + method.cls + " " + method.name + method.desc)
                } else {
                  println("Skipping lambda as bytecode and sourcecode occurrences mismatch: Missing usages:" + sourceLambdas.filter(!lambdas.contains(_)).map(l => l.cls + " " + l.lambdaId).mkString("(", ",", ")") + " for method " + method.cls + " " + method.name + method.desc)
                }
              }
            }
          }
        }
      }

      val outputWriter = Files.newBufferedWriter(set.valueOf(specOutput), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      GSON.toJson(new VersionedMDCDelegate(input.getFormatVersion, output), classOf[VersionedMappingDataContainer], outputWriter)
      outputWriter.write("\n")
      outputWriter.close()
    }
  }
}
