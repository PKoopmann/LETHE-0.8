package uk.ac.man.cs.lethe.internal.dl.conversion

abstract class Converter[SOURCE, TARGET] { 
  def convert(source: SOURCE): TARGET
}
