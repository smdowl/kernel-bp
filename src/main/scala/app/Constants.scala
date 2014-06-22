package app

object Constants {
  
  val orig = true
  val SUBDIR: String = if (orig) "orig/" else "vector-appended/"

  private final val BASE_DIR = "/Users/shaundowling/Google Drive/UCL/master project/code/kernel-bp/data/" + SUBDIR

  final val MINI_TRAIN_FILE = BASE_DIR + "mini.conll"
  final val MINI_TEST_FILE = BASE_DIR + "mini-test.conll"

  final val DEP_TEST = BASE_DIR + "dep-test.conll"
  final val TRAIN_FILE = BASE_DIR + "train.conll"

  final val CORRECT_DIR = "/Users/shaundowling/Google Drive/UCL/master project/code/kernelBP_source/kernelBP/"
}
