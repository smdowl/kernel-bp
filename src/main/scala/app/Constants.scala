package app

object Constants {
  
  val orig = false
  val SUBDIR: String = if (orig) "orig/" else "vector-appended/"

  private final val BASE_DIR = "/Users/Shaun/dev/kernel-bp/data/" + SUBDIR

  final val MICRO_TRAIN_FILE = BASE_DIR + "micro.conll"
  final val MINI_TRAIN_FILE = BASE_DIR + "mini.conll"
  final val MICRO_TEST_FILE = BASE_DIR + "micro-test.conll"
  final val MINI_TEST_FILE = BASE_DIR + "mini-test.conll"

  final val DEP_TEST = BASE_DIR + "dep-test.conll"
  final val TRAIN_FILE = BASE_DIR + "train.conll"
  final val DEV_FILE = BASE_DIR + "dev.conll"

  final val SMALL_TRAIN_FILE = BASE_DIR + "small-train.conll"
  final val SMALL_DEV_FILE = BASE_DIR + "small-dev.conll"

  final val MATLAB_DIR = "/Users/Shaun/dev/kernelBP_source/kernelBP/"
  final val CORRECT_DIR = MATLAB_DIR + "test-output/"
  final val SAMPLE_DATA = CORRECT_DIR + "sampArr"

  final val LOOPY_CORRECT_DIR = "/Users/Shaun/dev/kernelBP_source/kernelBP_loopy/test-output/"
  final val LOOPY_SAMPLE_DATA = LOOPY_CORRECT_DIR + "sampArr"

  final val OUTPUT_FILES_DIR = MATLAB_DIR + "scala-out/"

  final val INTER_MATRICES_DIR = MATLAB_DIR + "inter-out/"
}
