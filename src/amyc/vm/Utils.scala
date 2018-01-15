package amyc.vm

import amyc.wasm.Instructions.{Else, End, If_i32, If_void, Return}
import amyc.wasm.{Function, Instructions}


object Utils {
  // Given list of functions, positions main function before others, returns main function and new list
  def orderFunctions(unordered: List[Function]): (Function, List[Function]) = {
    val mainFunction: Function = unordered.filter(_.isMain).head
    val otherFunctions: List[Function] = unordered.filter(!_.isMain)
    (mainFunction, mainFunction :: otherFunctions)
  }

  // Given list of functions, returns a map that holds the starting index for each function name
  def mapFunctionsToIndices(functions: List[Function]): Map[String, Int] = {
    functions.map(function => (function.name, (function.code <:> Return).instructions))
      .flatMap(pair => pair._2.zipWithIndex.map { instrIndexPair =>
        if (instrIndexPair._2 == 0) (instrIndexPair._1, pair._1)
        else (instrIndexPair._1, "")
      }).zipWithIndex.filter(pair => pair._1._2 != "").map(pair => (pair._1._2, pair._2)).toMap
  }

  // Given list of functions, converts them to an array of instructions
  // and inserts Return instruction at the end of each function
  def functionsToInstructionArray(functions: List[Function]): Array[Instructions.Instruction] = {
    functions.flatMap(function => (function.code <:> Return).instructions).toArray
  }
  
  // Given the pointer of If instruction, returns the indices of matching Else and End instructions
  def findBoundariesOfIfInstruction(ifInstructionPointer: Int, instructionMemory: Array[Instructions.Instruction]): (Int, Int) = {
    // Find the end of if block
    var nestedCounter = 0
    var elseInstructionPointer = ifInstructionPointer

    // Iterate over the instructions between if and else until it finds the matching else instruction
    do {
      val currentInstruction = instructionMemory(elseInstructionPointer)
      if (currentInstruction == If_i32 || currentInstruction == If_void)
        nestedCounter = nestedCounter + 1
      else if (currentInstruction == Else)
        nestedCounter = nestedCounter - 1

      elseInstructionPointer = elseInstructionPointer + 1
    } while (nestedCounter != 0)

    // After this line, elseInstructionPointer points to Else instruction
    elseInstructionPointer = elseInstructionPointer - 1

    // Find the end of else block
    var endInstructionPointer = elseInstructionPointer

    // Iterate over the instructions between else and end until it finds the matching end instructions
    do {
      val currentInstruction = instructionMemory(endInstructionPointer)
      if (currentInstruction == Else)
        nestedCounter = nestedCounter + 1
      else if (currentInstruction == End)
        nestedCounter = nestedCounter - 1

      endInstructionPointer = endInstructionPointer + 1
    } while (nestedCounter != 0)

    // After this line, endInstructionPointer points to End instruction
    endInstructionPointer = endInstructionPointer - 1

    // Return the pair of indices
    (elseInstructionPointer, endInstructionPointer)
  }
}

