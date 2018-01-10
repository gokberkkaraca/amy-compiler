package amyc.vm

import amyc.codegen.Utils
import amyc.utils.{Context, Pipeline}
import amyc.wasm.Instructions._
import amyc.wasm._

import scala.io.StdIn

object VirtualMachine extends Pipeline[Module, Unit] {

  override def run(ctx: Context)(m: Module): Unit = {

    // Start the functions list with the main function
    val mainFunction: Function = m.functions.filter(_.isMain).head
    val otherFunctions: List[Function] = m.functions.filter(!_.isMain)
    val functions = mainFunction :: otherFunctions

    val functionNamesAndStartingIndices =
      functions.map(function => (function.name, (function.code <:> Return).instructions)).flatMap(pair => pair._2.zipWithIndex.map { instrIndexPair =>
        if (instrIndexPair._2 == 0) (instrIndexPair._1, pair._1)
        else (instrIndexPair._1, "")
      }).zipWithIndex.filter(pair => pair._1._2 != "").map(pair => (pair._1._2, pair._2)).toMap


    var labelsAndIndices: Map[String, Integer] = Map()

    var instructionsMemory: Array[Instructions.Instruction] = functions.flatMap(function => (function.code <:> Return).instructions).toArray
    var programCounter = 0

    var dataMemory: Array[Byte] = new Array[Byte](instructionsMemory.length)

    var mainStack: Array[Int] = new Array[Int](instructionsMemory.length)
    var msPointer = 0 // First empty space in mainStack

    var globals: Array[Int] = new Array[Int](10)

    var localsStack: Array[Int] = new Array[Int](instructionsMemory.length)
    val NUMBER_OF_DATA_FIELDS: Int = 4
    localsStack(0) = -1 // Return address for main
    localsStack(1) = 0 + 0 + NUMBER_OF_DATA_FIELDS // Caller offset + Caller number of locals + NUMBER_OF_DATA_FIELDS
    localsStack(2) = 0 // Number of locals of caller (which is 0 for main)
    localsStack(3) = mainFunction.locals // Number of locals of current function (main)
    var lsPointer = 0 // First empty space in localsStack

    while (true) {
      if (programCounter == -1)
        System.exit(0)

      val instruction: Instructions.Instruction = instructionsMemory(programCounter)
      programCounter = executeInstruction(instruction)
    }


    def executeInstruction(instruction: Instructions.Instruction): Int = {
      instruction match {

        // Load an int32 constant to the stack
        case Const(value) =>
          mainStack(msPointer) = value
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        // Numeric/logical instructions (all take i32 operands)
        case Add =>
          msPointer = msPointer - 1
          val value1 = mainStack(msPointer)

          msPointer = msPointer - 1
          val value2 = mainStack(msPointer)

          mainStack(msPointer) = value2 + value1
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        case Sub =>
          msPointer = msPointer - 1
          val value1 = mainStack(msPointer)

          msPointer = msPointer - 1
          val value2 = mainStack(msPointer)

          mainStack(msPointer) = value2 - value1
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        case Mul =>
          msPointer = msPointer - 1
          val value1 = mainStack(msPointer)

          msPointer = msPointer - 1
          val value2 = mainStack(msPointer)

          mainStack(msPointer) = value2 * value1
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        case Div =>
          msPointer = msPointer - 1
          val value1 = mainStack(msPointer)

          msPointer = msPointer - 1
          val value2 = mainStack(msPointer)

          mainStack(msPointer) = value2 / value1
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        case Rem =>
          msPointer = msPointer - 1
          val value1 = mainStack(msPointer)

          msPointer = msPointer - 1
          val value2 = mainStack(msPointer)

          mainStack(msPointer) = value2 % value1
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        case And =>
          msPointer = msPointer - 1
          val value1 = !(mainStack(msPointer) == 0)

          msPointer = msPointer - 1
          val value2 = !(mainStack(msPointer) == 0)

          mainStack(msPointer) = if (value2 && value1) 1 else 0
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        case Or =>
          msPointer = msPointer - 1
          val value1 = !(mainStack(msPointer) == 0)

          msPointer = msPointer - 1
          val value2 = !(mainStack(msPointer) == 0)

          mainStack(msPointer) = if (value2 || value1) 1 else 0
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        case Eqz =>
          msPointer = msPointer - 1
          val value = mainStack(msPointer)

          mainStack(msPointer) = if (value == 0) 1 else 0
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        case Lt_s =>
          msPointer = msPointer - 1
          val value1 = mainStack(msPointer)

          msPointer = msPointer - 1
          val value2 = mainStack(msPointer)

          mainStack(msPointer) = if (value2 < value1) 1 else 0
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        case Le_s =>
          msPointer = msPointer - 1
          val value1 = mainStack(msPointer)

          msPointer = msPointer - 1
          val value2 = mainStack(msPointer)

          mainStack(msPointer) = if (value2 <= value1) 1 else 0
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        case Eq =>
          msPointer = msPointer - 1
          val value1 = mainStack(msPointer)

          msPointer = msPointer - 1
          val value2 = mainStack(msPointer)

          mainStack(msPointer) = if (value2 == value1) 1 else 0
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        case Drop =>
          msPointer = msPointer - 1
          programCounter = programCounter + 1
          programCounter

        case Return =>
          programCounter = localsStack(lsPointer)
          lsPointer = lsPointer - localsStack(lsPointer + 2) - NUMBER_OF_DATA_FIELDS
          programCounter

        case If_i32 | If_void =>
          msPointer = msPointer - 1
          val ifConditionValue = mainStack(msPointer)
          val (elseInstruction, endInstruction) = findBoundariesOfIfInstruction()

          if (ifConditionValue != 0) {
            // Program Counter points to first instruction of if block
            programCounter = programCounter + 1
            var isBranch = false

            while (programCounter != elseInstruction && !isBranch) {
              val currentInstruction = instructionsMemory(programCounter)

              if (currentInstruction.isInstanceOf[Br])
                isBranch = true
              else
                programCounter = executeInstruction(currentInstruction)
            }

            if (instructionsMemory(programCounter).isInstanceOf[Br])
              programCounter = executeInstruction(instructionsMemory(programCounter))
            else
              programCounter = endInstruction + 1

            programCounter
          }
          else {
            // Program Counter points to first instruction of else block
            programCounter = elseInstruction + 1
            var isBranch = false

            while (programCounter != endInstruction && !isBranch) {
              val currentInstruction = instructionsMemory(programCounter)

              if (currentInstruction.isInstanceOf[Br])
                isBranch = true
              else
                programCounter = executeInstruction(currentInstruction)
            }

            if (instructionsMemory(programCounter).isInstanceOf[Br])
              programCounter = executeInstruction(instructionsMemory(programCounter))
            else
              programCounter = endInstruction + 1

            programCounter
          }

        case End =>
          programCounter = programCounter + 1
          programCounter

        case Loop(label) => labelsAndIndices += (label -> programCounter)
          programCounter = programCounter + 1
          programCounter

        case Br(label) =>
          programCounter = labelsAndIndices(label) + 1
          programCounter

        case Call(name) =>
          name match {
            case "Std_printInt" =>
              val value = mainStack(msPointer - 1)
              println(value)
              programCounter = programCounter + 1
              programCounter
            case "Std_printString" =>
              val start = mainStack(msPointer - 1)
              var stringVal = ""
              var stringCounter = start
              while (dataMemory(stringCounter) != 0) {
                stringVal = stringVal ++ dataMemory(stringCounter).toChar.toString
                stringCounter = stringCounter + 1
              }
              println(stringVal)
              programCounter = programCounter + 1
              programCounter
            case "Std_readInt" =>
              val value: Int = StdIn.readInt()
              mainStack(msPointer) = value
              msPointer = msPointer + 1
              programCounter = programCounter + 1
              programCounter
            case "Std_readString" =>
              val value: String = StdIn.readLine()
              val mkStringCode: Code = Utils.mkString(value)
              val previousProgramCounter = programCounter
              mkStringCode.instructions.foreach(executeInstruction)
              previousProgramCounter + 1

            case _ =>
              val targetFunction = functions.filter(_.name == name).head

              val returnAddress = programCounter + 1
              val offset = localsStack(lsPointer + 1)
              val numOfLocalsOfCaller = localsStack(lsPointer + 3)
              val numOfLocalsOfCallee = targetFunction.args + targetFunction.locals

              lsPointer = offset + numOfLocalsOfCaller
              localsStack(lsPointer) = returnAddress
              localsStack(lsPointer + 1) = offset + numOfLocalsOfCaller + NUMBER_OF_DATA_FIELDS
              localsStack(lsPointer + 2) = numOfLocalsOfCaller
              localsStack(lsPointer + 3) = numOfLocalsOfCallee

              var argCounter = targetFunction.args
              while (argCounter > 0) {
                msPointer = msPointer - 1
                val localValue = mainStack(msPointer)
                localsStack(argCounter + lsPointer + NUMBER_OF_DATA_FIELDS - 1) = localValue
                argCounter = argCounter - 1
              }

              functionNamesAndStartingIndices(targetFunction.name)
          }
        case Unreachable =>
          System.exit(-1)
          -1

        // Locals (parameters, local variables)
        case GetLocal(index) =>
          val value = localsStack(index + NUMBER_OF_DATA_FIELDS + lsPointer)
          mainStack(msPointer) = value
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        case SetLocal(index) =>
          msPointer = msPointer - 1
          val value = mainStack(msPointer)
          localsStack(index + NUMBER_OF_DATA_FIELDS + lsPointer) = value
          programCounter = programCounter + 1
          programCounter


        // Global variables
        case GetGlobal(index) =>

          mainStack(msPointer) = globals(index)
          msPointer = msPointer + 1
          programCounter = programCounter + 1
          programCounter

        case SetGlobal(index) =>
          msPointer = msPointer - 1
          val value = mainStack(msPointer)
          globals(index) = value
          programCounter = programCounter + 1
          programCounter


        // Memory
        // Stores an i32 to memory. Expects memory address, then stored value as operands
        case Store =>
          msPointer = msPointer - 1
          val value: Int = mainStack(msPointer)

          msPointer = msPointer - 1
          val address: Int = mainStack(msPointer)
          dataMemory(address + 3) = ((value & 0xFF000000) >> 3 * 8).toByte
          dataMemory(address + 2) = ((value & 0x00FF0000) >> 2 * 8).toByte
          dataMemory(address + 1) = ((value & 0x0000FF00) >> 1 * 8).toByte
          dataMemory(address + 0) = ((value & 0x000000FF) >> 0 * 8).toByte

          programCounter = programCounter + 1
          programCounter

        // Loads an i32 to memory. Expects memory address as operand
        case Load =>
          msPointer = msPointer - 1
          val address: Int = mainStack(msPointer)

          val byte3: Int = (dataMemory(address + 0) & 0xFF).toInt << 0
          val byte2: Int = (dataMemory(address + 1) & 0xFF).toInt << 8
          val byte1: Int = (dataMemory(address + 2) & 0xFF).toInt << 16
          val byte0: Int = (dataMemory(address + 3) & 0xFF).toInt << 24


          val value = byte0 + byte1 + byte2 + byte3


          mainStack(msPointer) = value
          msPointer = msPointer + 1

          programCounter = programCounter + 1
          programCounter

        // Stores a single byte to memory (the least significant byte of the operand)
        // Operands expected are like Store
        case Store8 =>
          msPointer = msPointer - 1
          val value: Int = mainStack(msPointer)
          val leastSignificantByte: Int = value & 0x000000FF

          msPointer = msPointer - 1
          val address: Int = mainStack(msPointer)

          dataMemory(address) = leastSignificantByte.toByte

          programCounter = programCounter + 1
          programCounter

        // Load a byte from memory, then zero-extend it to fill an i32
        case Load8_u =>
          msPointer = msPointer - 1
          val address: Int = mainStack(msPointer)

          val value: Int = dataMemory(address) & 0x000000FF
          mainStack(msPointer) = value
          msPointer = msPointer + 1

          programCounter = programCounter + 1
          programCounter
      }
    }

    def findBoundariesOfIfInstruction(): (Int, Int) = {
      // Find the end of if block
      var nestedCounter = 0
      var endOfIfBlockPointer = programCounter
      do {
        val currentInstruction = instructionsMemory(endOfIfBlockPointer)
        if (currentInstruction == If_i32 || currentInstruction == If_void)
          nestedCounter = nestedCounter + 1
        else if (currentInstruction == Else)
          nestedCounter = nestedCounter - 1

        endOfIfBlockPointer = endOfIfBlockPointer + 1
      } while (nestedCounter != 0)

      // endOfIfBlockPointer points to Else instruction
      endOfIfBlockPointer = endOfIfBlockPointer - 1

      var endOfElseBlockPointer = endOfIfBlockPointer
      do {
        val currentInstruction = instructionsMemory(endOfElseBlockPointer)
        if (currentInstruction == Else)
          nestedCounter = nestedCounter + 1
        else if (currentInstruction == End)
          nestedCounter = nestedCounter - 1

        endOfElseBlockPointer = endOfElseBlockPointer + 1
      } while (nestedCounter != 0)

      // endOfElseBlockPointer points to End instruction
      endOfElseBlockPointer = endOfElseBlockPointer - 1

      (endOfIfBlockPointer, endOfElseBlockPointer)
    }
  }
}

