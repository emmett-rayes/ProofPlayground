package proofPlayground
package frontend.notation

import frontend.notation.parser.Parser

import scala.reflect.ClassTag

object TracingParser:
  private var level = -1
  extension [Input, Output: ClassTag](self: Parser[Input, Output])
    def trace: Parser[Input, Output] =
      input =>
        level += 1
        val indent             = if level == 0 then "" else "|--" * (level - 1) + "|--"
        val childIndent        = "|--" * level + "|--"
        val childIndentPlusOne = "|--" * (level + 1)
        val output             = summon[ClassTag[Output]].runtimeClass.getSimpleName
        println(s"${indent}Attempting to parse ${output} on input \"$input\"")
        val result = self.parse(input)
        if result.isSuccess then
          println(s"${childIndent}Successfully parsed ${output}")
          println(s"${childIndentPlusOne}Remaining: \"${result.get.remaining}\"")
          println(s"${childIndentPlusOne}Parsed:    ${result.get.parsed}")
        else
          println(s"${childIndent}Failed to parse ${output}")
          println(s"${childIndentPlusOne}Reason: ${result.failed.get.getMessage}")
        level -= 1
        result
