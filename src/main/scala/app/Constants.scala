package app

object Constants {
  
  val orig = true
  val SUBDIR: String = if (orig) "orig/" else "vector-appended/"

  private final val BASE_DIR = "/Users/Shaun/dev/kernel-bp/data/" + SUBDIR

  final val MINI_TRAIN_FILE = BASE_DIR + "mini.conll"
  final val MINI_TEST_FILE = BASE_DIR + "mini-test.conll"

  final val DEP_TEST = BASE_DIR + "dep-test.conll"
  final val TRAIN_FILE = BASE_DIR + "train.conll"

  final val MATLAB_DIR = "/Users/Shaun/dev/kernelBP_source/kernelBP/"
  final val CORRECT_DIR = MATLAB_DIR + "test-output/"
  final val SAMPLE_DATA = CORRECT_DIR + "sampArr"

  final val OUTPUT_FILES_DIR = MATLAB_DIR + "scala-out/"
}
