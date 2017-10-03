package amyc.utils

// Contains a reporter and configuration for the compiler
case class Context(
  reporter: Reporter,
  files: List[String]
)
