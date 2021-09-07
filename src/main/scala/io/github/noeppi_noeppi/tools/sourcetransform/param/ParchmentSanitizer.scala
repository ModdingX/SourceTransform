package io.github.noeppi_noeppi.tools.sourcetransform.param

import com.google.gson.{Gson, GsonBuilder}
import io.github.noeppi_noeppi.tools.sourcetransform.inheritance.{InheritanceMap, LambdaInfo, MethodInfo}
import io.github.noeppi_noeppi.tools.sourcetransform.util.{LanguageLevel, SourceUtil, Util}
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
import scala.jdk.CollectionConverters._

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
    val specLevel = options.acceptsAll(List("l", "level").asJava, "Source level").withRequiredArg().withValuesConvertedBy(Util.enum[LanguageLevel]).defaultsTo(LanguageLevel.JAVA_16)
    val specInput = options.acceptsAll(List("m", "input").asJava, "Input for the parchment export to process.").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specOutput = options.acceptsAll(List("o", "output").asJava, "Output for the sanitized parchment export.").withRequiredArg().withValuesConvertedBy(new PathConverter())
    val specQuiet = options.acceptsAll(List("q", "quiet").asJava, "Suppress warning message while reading source code.")
    val specIgnore = options.acceptsAll(List("b", "ignore").asJava, "Packages to ignore (with all subpackages).").withRequiredArg().withValuesSeparatedBy(',')
    val set = try {
      options.parse(args: _*)
    } catch {
      case e: OptionException => System.err.println("Option exception: " + e.getMessage); options.printHelpOn(System.err); Util.exit(0)
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
      val inheritance = InheritanceMap.read(inheritanceReader)
      inheritanceReader.close()

      val ignored = set.valuesOf(specIgnore).asScala.map(_.replace('.', '/')).toSet
      val ignoredStart = ignored.map(_ + "/")

      val base = set.valueOf(specSources).toAbsolutePath.normalize()
      val files = SourceUtil.getJavaSources(base).filter(f => !ignored.contains(f) && !ignoredStart.exists(f.startsWith))
      val createParser = SourceUtil.createParserFactory(set.valueOf(specLevel), base, set.valuesOf(specClasspath).asScala.toSeq) _
      
      val executor = new ScheduledThreadPoolExecutor(1 max (Runtime.getRuntime.availableProcessors() - 1), (action: Runnable) => {
        val thread = new Thread(action)
        thread.setDaemon(true)
        thread
      })
      val sourceFilesBetweenNotify = 1 max (100 min (files.size / 20))
      var completed = 0
      val lock = new AnyRef
      val futures = ListBuffer[Future[_]]()
      
      val mapBuilder = mutable.Map[MethodInfo, ParamSanitizer]()
      val lambdaBuilder = mutable.Map[LambdaInfo, LambdaTarget]()
      files.foreach(file => {
        futures.addOne(executor.submit(new Runnable {
          override def run(): Unit = {
            val currentMap = mutable.Map[MethodInfo, ParamSanitizer]()
            val lambdaMap = mutable.Map[LambdaInfo, LambdaTarget]()
            try {
              val parser = createParser(file)
              parser.createAST(null) match {
                case cu: CompilationUnit =>
                  val visitor = new SourceVisitor(inheritance, currentMap, lambdaMap, set.has(specQuiet))
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
              completed += 1
              if (completed % sourceFilesBetweenNotify == 0) {
                val percentage = Math.round(100 * (completed / files.size.toDouble))
                if (percentage < 100) {
                  System.err.println(s"$percentage% ($completed / ${files.size})")
                }
              }
            }
          }
        }))
      })
      futures.foreach(_.get())
      executor.shutdownNow()
      System.err.println(s"100% (${files.size} / ${files.size})")
      
      val map = mapBuilder.toMap
      val lambdas = lambdaBuilder.toMap
      
      val inputReader = Files.newBufferedReader(set.valueOf(specInput))
      val input = GSON.fromJson(inputReader, classOf[VersionedMappingDataContainer])
      inputReader.close()
      
      val output = new MappingDataBuilder

      input.getPackages.forEach(pkg => output.createPackage(pkg.getName).addJavadoc(pkg.getJavadoc))

      input.getClasses.forEach(cls => {
        val cb = output.createClass(cls.getName).addJavadoc(cls.getJavadoc)
        cls.getFields.forEach(fd => cb.createField(fd.getName, fd.getDescriptor).addJavadoc(fd.getJavadoc))
        // Sanitizers after param mappings have been applied. Required for lambdas so lambda parameters
        // don't conflict with method parameters.
        val renamedMappers = mutable.Map[MethodInfo, ParamSanitizer]()
        cls.getMethods.forEach(md => {
          val info = MethodInfo(cls.getName, md.getName, md.getDescriptor)
          // First process non-lambdas as for processing lambdas we need the renamed parameters
          if (!md.getName.startsWith("lambda$")) {
            val mapper = map.getOrElse(info, ParamSanitizer.queryDefault(info, set.has(specQuiet)))
            val renames = mutable.Set[String]()
            val mb = cb.createMethod(md.getName, md.getDescriptor).addJavadoc(md.getJavadoc)
            md.getParameters.forEach(param => {
              val newName = mapper.sanitize(param.getName)
              mb.createParameter(param.getIndex).setName(newName).setJavadoc(param.getJavadoc)
              renames.addOne(newName)
            })
            renamedMappers.put(info, ParamSanitizer.withRenamed(mapper, renames.toSet))
          }
        })
        cls.getMethods.forEach(md => {
          val info = MethodInfo(cls.getName, md.getName, md.getDescriptor)
          // Now to the lambdas
          if (md.getName.startsWith("lambda$")) {
            val sourceLambdas = inheritance.getLambdasFor(info)
            // Skip lambdas where not all occurrences are found in source code.
            if (sourceLambdas.nonEmpty && sourceLambdas.forall(l => lambdas.contains(l))) {
              val sourceLambdaTargets = sourceLambdas.flatMap(lambdas.get)
              val factory: MethodInfo => ParamSanitizer = m => renamedMappers.getOrElseUpdate(m, map.getOrElse(m, ParamSanitizer.queryDefault(info, set.has(specQuiet))))
              val mapperOptions: Seq[Option[ParamSanitizer]] = sourceLambdaTargets.toSeq.map(t => t.resolve(factory))
              // Check that the implementation method matches at least one lambda usage
              // If the same lambda method is used twice, it'll get the name of the first occurrence
              // So in case of some compiler weirdness, we have an additional safety point with this.
              if (!sourceLambdaTargets.exists(target => target.canMatch(md.getName))) {
                println("Skipping lambda as bytecode and sourcecode name mismatch: For lambda:" + md.getName + ", targets: " + sourceLambdaTargets.flatMap(_.lambdaName()).mkString("(", ",", ")"))
              } else if (mapperOptions.forall(_.isDefined)) {
                // If one is not defined, it was set to skip
                // which causes the entire lambda to skip.
                ParamSanitizer.queryLambda(info, set.has(specQuiet), mapperOptions.map(_.get): _*) match {
                  case Some(mapper) =>
                    val mb = cb.createMethod(md.getName, md.getDescriptor).addJavadoc(md.getJavadoc)
                    md.getParameters.forEach(param => {
                      mb.createParameter(param.getIndex).setName(mapper.sanitize(param.getName)).setJavadoc(param.getJavadoc)
                    })
                  case None =>
                }
              }
            } else {
              if (!set.has(specQuiet)) {
                if (sourceLambdas.isEmpty) {
                  println("Skipping lambda-like method as it has no known use as a lambda: " + info.cls + " " + info.name + info.signature)
                } else {
                  println("Skipping lambda as bytecode and sourcecode occurrences mismatch: Missing usages:" + sourceLambdas.filter(!lambdas.contains(_)).map(l => l.cls + " " + l.lambdaId).mkString("(", ",", ")") + " for method " + info.cls + " " + info.name + info.signature)
                }
              }
            }
          }
        })
      })
      
      val outputWriter = Files.newBufferedWriter(set.valueOf(specOutput), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
      GSON.toJson(new VersionedMDCDelegate(input.getFormatVersion, output), classOf[VersionedMappingDataContainer], outputWriter)
      outputWriter.write("\n")
      outputWriter.close()
    }
  }
}
