package amyc.vm

object Config {

  // Sizes for VM structures
  val SIZE_OF_MAIN_STACK: Int = 1000000
  val SIZE_OF_CALL_STACK: Int = 1000000
  val SIZE_OF_DATA_MEMORY: Int = 1000000
  val NUMBER_OF_GLOBALS: Int = 10

  // Number of slots used by each call frame, excluding the locals
  val NUMBER_OF_FRAME_DATA_FIELDS: Int = 3

  // Sentinel value signifying end of program
  val END_OF_PROGRAM: Int = -1

  // Error code for Unreachable instruction
  val UNREACHABLE_CODE: Int = -1
}
