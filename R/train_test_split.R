#' Train test split
#'
#' Split a dataframe into two dataframes, one for testing and one for training. Very similar to sklearn.model_selection.train_test_split
#'
#' @param data A dataframe to split into test and train
#' @param test.size Either an integer indicating the number of rows that should be in test, or a percentage (expressed as a number between 0 and 1) of the number of rows from data that should be in test
#' @param train.size Either an integer indicating the number of rows that should be in train, or a percentage (expressed as a number between 0 and 1) of the number of rows from data that should be in train
#' @param shuffle A boolean indicating if the dataframe should be shuffled, defaults to TRUE
#'
#' @return A named list where 'test' is the test dataframe and 'train' is the train dataframe
#'
#' @importFrom utils head
#' @importFrom utils tail
#' @export
#'
#' @examples
#' data("mtcars")
#' train.test.split(mtcars, train.size = 0.7)
#' train.test.split(mtcars, test.size = 14)
train.test.split <- function(data,
                             test.size = 0,
                             train.size = 0,
                             shuffle = TRUE
) {
  if (test.size == 0 && train.size == 0 && nrow(data) < 2) {
    stop("test.size and train.size cannot both be 0")
  }

  if (shuffle) {
    data <- data[sample(nrow(data)),]
  }

  n <- 0

  if (!test.size == 0 && test.size %% 1 == 0) {
    n <- test.size
  }
  if (!test.size == 0 && !test.size %% 1 == 0) {
    n <- round(test.size * nrow(data))
    if (n == 0) {
      stop("test.size too small, 0 rows in test dataframe")
    }
  }
  if (!train.size == 0 && train.size %% 1 == 0) {
    n <- nrow(data) - train.size
  }
  if (!train.size == 0 && !train.size %%1 == 0) {
    n <- nrow(data) - round(train.size * nrow(data))
    if (n == 0) {
      stop("train.size too small, 0 rows in train dataframe")
    }
  }

  test_df <- head(data, n)
  train_df <- tail(data, nrow(data) - n)

  list("test" = test_df, "train" = train_df)
}
