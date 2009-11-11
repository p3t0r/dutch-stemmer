package com.log4p

/**
 * collection of functions which process the given input as a pipeline
 */
case class Pipeline[T](processes:(T=>T)*) {
  def exec(input:T):T = {
    processes.foldLeft(input) {
        (previousOutput, step) => step.apply(previousOutput)
    }
  }
}