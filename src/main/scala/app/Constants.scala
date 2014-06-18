package app

object Constants {
  private final val ORIG_DIR = "/Users/shaundowling/Google Drive/UCL/master project/code/history-gen/data/orig/"
  private final val VECTOR_DIR = "/Users/shaundowling/Google Drive/UCL/master project/code/history-gen/data/vector-appended/"

  private final val BASE_DIR = ORIG_DIR

  final val MINI_TRAIN_FILE = BASE_DIR + "mini.conll"
  final val MINI_TEST_FILE = BASE_DIR + "mini-test.conll"

  final val DEP_TEST = BASE_DIR + "dep-test.conll"
  final val TRAIN_FILE = BASE_DIR + "train.conll"
}
