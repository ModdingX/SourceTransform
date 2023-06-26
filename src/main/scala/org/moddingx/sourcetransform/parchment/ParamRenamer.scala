package org.moddingx.sourcetransform.parchment

sealed trait ParamRenamer {
  // source is only a Some when the inheritance-params option is set.
  def rename(param: String, source: Option[String]): String
  def withExcludedNames(names: Set[String]): ParamRenamer
}

object ParamRenamer {
  
  def merge(main: ParamRenamer, additional: Set[ParamRenamer]): ParamRenamer = {
    def excludedNames(renamer: ParamRenamer): Set[String] = renamer match {
      case Default(excludedNames, _, _) => excludedNames
      case Keep => Set()
      case Always(_) => throw new IllegalArgumentException("ALWAYS param renamer has no excluded names")
    }
    
    def excludedSourceNames(renamer: ParamRenamer): Set[String] = renamer match {
      case Default(_, excludedSourceNames, _) => excludedSourceNames
      case Keep => Set()
      case Always(_) => throw new IllegalArgumentException("ALWAYS param renamer has no excluded source names")
    }

    def localClassLevel(renamer: ParamRenamer): Int = renamer match {
      case Default(_, _, localClassLevel) => localClassLevel
      case Keep => 0
      case Always(localClassLevel) => localClassLevel
    }
    
    main match {
      case r @ Always(_) => r
      case _ if additional.exists(_.isInstanceOf[Always]) => Always(additional.map(localClassLevel).max)
      case _ if additional.isEmpty => main
      case Default(mainExcludedNames, mainExcludedSourceNames, localClassLevel) => Default(mainExcludedNames | additional.flatMap(excludedNames), mainExcludedSourceNames | additional.flatMap(excludedSourceNames), localClassLevel)
      case Keep => Default(additional.flatMap(excludedNames), additional.flatMap(excludedSourceNames), additional.map(localClassLevel).max)
    }
  }
  
  val Fallback: ParamRenamer = Always(0)
  
  // Treat every name as potentially used, sanitize everything
  case class Always(localClassLevel: Int) extends ParamRenamer {
    override def rename(param: String, source: Option[String]): String = "p_" + param + ("_" * localClassLevel)
    override def withExcludedNames(names: Set[String]): ParamRenamer = this
  }
  
  case object Keep extends ParamRenamer {
    override def rename(param: String, source: Option[String]): String = param
    override def withExcludedNames(names: Set[String]): ParamRenamer = Default(names, Set(), 0)
  }
  
  case class Default(excludedNames: Set[String], excludedSourceNames: Set[String], localClassLevel: Int) extends ParamRenamer {
    
    override def rename(param: String, source: Option[String]): String = {
      val baseParam = (if param.endsWith("_") then param + "0" else param) + ("_" * localClassLevel)
      
      // If no source names are excluded, we don't need to check the source name
      val isExcludedDueToSourceName = if (excludedSourceNames.isEmpty) false else {
        source.isEmpty || excludedSourceNames.contains(source.get)
      }
      
      if (!excludedNames.contains(baseParam) && !isExcludedDueToSourceName) {
        baseParam
      } else {
        var sanitized = "p_" + baseParam
        while (excludedNames.contains(sanitized)) sanitized = "p" + sanitized
        sanitized
      }
    }

    override def withExcludedNames(names: Set[String]): ParamRenamer = Default(excludedNames | names, excludedSourceNames, localClassLevel)
  }
}

