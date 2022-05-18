# Create classes for saving the soundscape objects at the various stages
# of the soundscapeR workflow

  # 1. An aggregated_soundscape class

setClass("soundscape",
         representation(first_day="POSIXct",
                        lat="numeric",
                        lon="numeric",
                        tz="character",
                        sunrise="POSIXct",
                        sunset="POSIXct",
                        fileloc="character",
                        index="character",
                        samplerate="numeric",
                        window="numeric",
                        binarization_method = "character",
                        threshold = "numeric",
                        output="character",
                        merged_df="data.frame",
                        binarized_df = "data.frame",
                        aggregated_df="data.frame",
                        aggregated_df_per_time="list",
                        effort_per_time="list"),
         prototype(first_day = NA_character_,
                   lat = NA_real_,
                   lon = NA_real_,
                   tz = NA_character_,
                   sunrise = NA_character_,
                   sunset = NA_character_,
                   fileloc = NA_character_,
                   index = NA_character_,
                   samplerate = NA_real_,
                   window = NA_real_,
                   binarization_method = NA_character_,
                   threshold = NA_real_,
                   output = NA_character_,
                   merged_df = data.frame(a=NA, b=NA),
                   binarized_df = data.frame(a="missing"),
                   aggregated_df = data.frame(a="missing"),
                   aggregated_df_per_time = as.list(rep(NA, 10)),
                   effort_per_time = as.list(rep(NA, 10))))

setMethod("show",
          "soundscape",
          function(object) {
            cat("\n")
            cat(crayon::bold("1. ",
                             crayon::underline("Soundscape metadata"),
                             "\n"))
            cat("\n")
            cat(crayon::bold("    Sampling point metadata: ", "\n"))
            cat("\n")
            cat("    First day of recording: ",
                as.character(object@first_day),
                "\n")
            cat("    Latitude of sampling point: ", object@lat, "\n")
            cat("    Longitude of sampling point: ", object@lon, "\n")
            cat("    Time zone of sampling point: ", object@tz, "\n")
            cat("    Sunrise time at sampling point: ",
                as.character(hms::as_hms(round(object@sunrise))),
                "\n")
            cat("    Sunset time at sampling point: ",
                as.character(hms::as_hms(round(object@sunset))),
                "\n")
            cat("\n")
            cat(crayon::bold("    Acoustic index metadata: ", "\n"))
            cat("\n")
            cat("    Path to raw sound files: ",
                object@fileloc,
                "\n")
            cat("    Spectral index used: ",
                object@index,
                "\n")
            cat("    Sampling rate of the recording: ",
                object@samplerate,
                " Hz",
                "\n")
            cat("    Window size used in FFT: ",
                object@window,
                " samples",
                "\n")
            cat("    Frequency resolution: ",
                object@samplerate/object@window,
                " Hz",
                "\n")
            cat("    Temporal resolution: ",
                object@window/object@samplerate,
                " ms",
                "\n")
            cat("\n")

            if(object@binarized_df[1, 1]=="missing"){

              cat(crayon::bold("    Workflow update: ",
                               "\n"))
              cat("\n")
              cat(crayon::red("    The workflow goes as follows:", crayon::bold(" Merge -"), "Binarize -", "Aggregate"))

              cat("\n")

              cat(crayon::red("    The binarization step has not yet been performed.",
                  "\n"))

              cat(crayon::red("    It appears your next step is: 'binarize_df()'",
                  "\n"))
            }

            else{

              cat(crayon::bold("    Data frame binarization metadata: ", "\n"))
              cat("\n")
              cat("    Used binarization algorithm: ",
                  object@binarization_method,
                  "\n")
              cat("    Binarization threshold: ",
                  object@threshold,
                  "\n")

            }

            cat("\n")

            if(object@aggregated_df[1, 1]=="missing"){

              if(object@binarized_df[1, 1]=="missing"){
              }

              else{

                cat(crayon::bold("    Workflow update: ",
                                 "\n"))
                cat("\n")
                cat(crayon::red("    The workflow goes as follows:", crayon::bold(" Merge -", "Binarize -"), "Aggregate"))

                cat("\n")

                cat(crayon::red("    The aggregation step has not yet been performed.",
                                "\n"))

                cat(crayon::red("    It appears your next step is: 'aggregate_df()'",
                                "\n"))

              }
            }

            else{

              cat(crayon::bold("    Aggregated data frame metadata: ",
                               "\n"))
              cat("\n")
              cat("    Output format: ", object@output,
                  "\n")
              cat("    Data frame frequency range: ",
                  min(
                    as.numeric(
                      rownames(
                        object@aggregated_df))), "-",
              max(
                as.numeric(
                  rownames(
                    object@aggregated_df))), "Hz",
              "\n")

              cat("    Data frame time range: ",
                  min(
                    as.character(
                      hms::as_hms(
                      colnames(
                        object@aggregated_df)))), "-",
                  max(
                    as.character(hms::as_hms(
                      colnames(
                        object@aggregated_df)))),
                  "\n")

            }

            cat("\n")
            cat(crayon::bold("2. ",
                             crayon::underline("Soundscape data"),
                             "\n"))
            cat("\n")
            cat(crayon::bold("    Merged data frame data: ", "\n"))
            cat("\n")
            cat("Columns 1 to 5 and rows 1 to 5 displayed", "\n")
            cat("\n")
            print(head(object@merged_df)[1:5,1:5])
            cat("\n")

            if(object@binarized_df[1, 1]=="missing"){

              cat(crayon::bold("    Binarized data frame data: ", "\n"))
              cat("\n")

              cat(crayon::red("    The binarization step has not yet been performed.",
                  "\n"))

            }

            else{
              cat(crayon::bold("    Binarized data frame data: ", "\n"))
              cat("\n")
              cat("Columns 1 to 5 and rows 1 to 5 displayed", "\n")
              cat("\n")
              print(head(object@binarized_df)[1:5,1:5])

            }

            cat("\n")
            cat(crayon::bold("    Aggregated data frame data: ", "\n"))
            cat("\n")

            if(object@aggregated_df[1, 1]=="missing"){

              cat(crayon::red("    The aggregation step has not yet been performed."))

            }

            else{

            cat("   ",
                crayon::underline("Aggregated data frame:"),
                "\n")
            cat("\n")
            cat("Columns 1 to 5 and rows 1 to 5 displayed", "\n")
            cat("\n")
            print(head(object@aggregated_df)[1:5,1:5])
            cat("\n")
            cat("   ",
                crayon::underline("Aggregated data frame per time:"),
                "\n")
            cat("\n")
            cat("First list element displayed: ",
                colnames(object@aggregated_df_per_time[[1]])[1],
                "\n")
            cat("\n")
            cat("Columns 1 to 5 and rows 1 to 5 displayed", "\n")
            cat("\n")
            print(object@aggregated_df_per_time[[1]][1:5, 1:5])
            cat("\n")
            cat("   ",
                crayon::underline("Number of soundscape samples per time (sampling effort):"),
                "\n")
            cat("\n")
            cat("List elements 1 to 5 displayed", "\n")
            cat("\n")
            print(object@effort_per_time[1:5])

            }
          }
)


# Functions for calculating acoustic indices
# !) Prior to running this command, make sure you have:

# 1) Installed the latest version of AnalysisPrograms.exe on:
# https://github.com/QutEcoacoustics/audio-analysis/releases
# 2) Renamed your files to abide by the following naming rules:
# https://ap.qut.ecoacoustics.info/basics/dates.html

#' Set Configuration Parameters for Acoustic Index Computation
#'
#' @description Alters and saves the configuration file used by
#' external software 'AnalysisPrograms.exe' for the computation of
#' acoustic indices.
#'
#' @param progloc The full-length path to the location of the
#' 'AnalysisPrograms.exe' software.
#' @param samplerate The number of times the sound was sampled each second.
#' This is a fixed parameter determined by your recording setup, although
#'  downsampling to a lower sampling rate is possible.
#' @param window A variable of the Short-time Fourier Transformation,
#' expressed as the number of samples. The window size of choice
#' depends on the fundamental frequency, intensity and change of the signal
#' of interest, and influences the temporal and frequency resolution
#' of the analysis. The window size is generally a power of 2.
#'
#' @details The \code{index_config()} function is normally only used internally
#' by the \code{index_calc()} function, although it is possible for the
#' user to edit the configuration file manually.
#'
#' @return Edits and saves the '.yml' configuration file which is
#' used by the AnalysisPrograms software for index computation.
#'
#' @export
index_config <- function(progloc, samplerate = 41000, window = 256) {

  # 1. Check if function inputs meet expectations

  # 1.1. progloc is an existing, writable directory

  test_1 <- function(x){
    assertthat::is.dir(x)
  }

  assertthat::on_failure(test_1) <- function(call, env) {

    paste0(deparse(call$x), " is not an existing directory. If you're using a windows device, make sure to change backslashes to forwards slashes in the path.")

  }

  assertthat::assert_that(test_1(progloc))

  test_1_1 <- function(x){
    assertthat::is.writeable(x)
  }

  assertthat::on_failure(test_1_1) <- function(call, env) {

    paste0(deparse(call$x), " is not a writable directory. Change permissions.")

  }

  assertthat::assert_that(test_1_1(progloc))

  # 1.2. samplerate is a single, positive integer

  test_2 <- function(x){
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " is not a single, positive integer. Consult the package documentation for more information on the samplerate.")

  }

  assertthat::assert_that(test_2(samplerate))

  # 1.3. window is a single, positive integer

  test_3 <- function(x){
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is not a single, positive integer. Consult the package documentation for more information on the window")

  }

  assertthat::assert_that(test_3(window))

  # 1.4. produce a warning message if window is not a power of two

  if(!as.integer(log(window, base=2)) == (log(window, base = 2))){
    cat("\n Chosen window size is not a power of two. This is a warning message, \n if your window size was chosen purposefully, proceed as planned by pressing Y. \n If you want to abort, press N.", "\n")
    Sys.sleep(0.000000000001)

    question1 <- readline("Would you like to proceed with the chosen window size? (Y/N)")

    if(regexpr(question1, 'y', ignore.case = TRUE) == 1){
    }
    else{

      if(regexpr(question1, 'n', ignore.case = TRUE) == 1){
        print("Index computation aborted.")
        Sys.sleep(0.0000000000000000000000000001)
        stop()
      } else {
        print("The option you have chosen is not valid - index computation aborted")

        Sys.sleep(0.00000000000000000000000000000001)
        stop()
      }
    }

  }

  # 2. Import the yaml file into R, and check


  config <- as.data.frame(
    yaml::read_yaml(file = paste0(
      progloc, "/ConfigFiles/Towsey.Acoustic.yml"
    ))
  )

  # 2.1. Check if the configuration file can be read and is writable

  test_5_1 <- function(x){

    assertthat::is.writeable(x)
    assertthat::is.readable(x)

  }

  assertthat::on_failure(test_5_1) <- function(call, env){

    paste0(deparse(call$x), " the configuration file is not writable. Change permissions.")

  }

  assertthat::assert_that(test_5_1(
    paste0(progloc, "/ConfigFiles/Towsey.Acoustic.yml")))

  # 2.1. check if yaml config file succesfully imported as dataframe

  test_5_2 <- function(x){
    is.data.frame(x)
  }

  assertthat::on_failure(test_5_2) <- function(call, env){

    paste0(deparse(call$x), " is not a dataframe. The yaml configuration file could not be imported succesfully.")

  }

  assertthat::assert_that(test_5_2(config))

  # 3. Edit configuration file and check

  config$SegmentDuration <- 60
  config$IndexCalculationDuration <- 60
  config$ResampleRate <- as.integer(samplerate)
  config$FrameLength <- as.integer(window)

  # 3.1. check if the segment duration was succesfully changed to 60 seconds

  test_6 <- function(x){
    assertthat::are_equal(x, 60)
  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " could not be changed to 60 seconds.")

  }

  assertthat::assert_that(test_6(config$SegmentDuration))

  # 3.2. check if the index calculation duration was successfully changed to 60 seconds.

  test_7 <- function(x){
    assertthat::are_equal(x, 60)
  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " could not be changed to 60 seconds.")

  }

  assertthat::assert_that(test_7(config$IndexCalculationDuration))

  # 3.3. check if the sampling rate was successfully changed to the specified value.

  test_8 <- function(x){
    assertthat::are_equal(x, as.integer(samplerate))
  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " could not be changed to the specified sampling rate.")

  }

  assertthat::assert_that(test_8(config$ResampleRate))

  # 3.4. check if the window was successfully changed to the specified value.

  test_9 <- function(x){
    assertthat::are_equal(x, as.integer(window))
  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " could not be changed to the specified window size.")

  }

  assertthat::assert_that(test_9(config$FrameLength))

  # 4. Overwrite the original yaml file with the edited version


  yaml::write_yaml(config, file <- paste0(
    progloc, "/ConfigFiles/Towsey.Acoustic.Custom.yml"
  ))

  # 4.1. check if configuration file successfully changed.

  config_2 <- as.data.frame(
    yaml::read_yaml(file = paste0(
      progloc, "/ConfigFiles/Towsey.Acoustic.Custom.yml"
    ))
  )

  test_10 <- function(x){

    assertthat::are_equal(x, config)

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " could not be succesfully overwrite previous configuration file.")

  }

  assertthat::assert_that(test_10(config_2))

}


#' Compute Acoustic Indices
#'
#' @description Calls on the external software 'AnalysisPrograms.exe'
#' to compute a set of acoustic indices using user-specified
#' configuration parameters.
#'
#' @param fileloc The full-length path to the sound file for which to
#' compute indices.
#' @param progloc The full-length path to the location of
#' 'AnalysisPrograms.exe'.
#' @param samplerate The number of times the sound was sampled each second.
#' This is a fixed parameter determined by your recording setup, although
#' downsampling to a lower sampling rate is possible.
#' @param window A variable of the Short-time Fourier Transformation,
#' expressed as the number of samples. The window size of choice depends on
#' the fundamental frequency, intensity and change of the signal of interest,
#' and influences the temporal and frequency resolution of the analysis.
#' The window size is generally a power of 2.
#' @param parallel A boolean flag (TRUE of FALSE) indicating whether parallel processing
#' should be enables for index computation. Set to FALSE by default.
#'
#' @details
#' The default duration of sound files for index computation is 60 seconds.
#' If the file length exceeds 60 seconds, the file is automatically cut into
#' 60 second segments for further analysis.
#'
#' \bold{The following spectral indices will be computed:}
#'
#' \emph{Background Noise (BGN)}:
#' The mode of the distribution of decibel values in each
#' frequency bin, representing the background intensity value.
#' This index captures the acoustic energy which persists throughout the
#' duration of the sound file, regardless of its origin
#' (biophonic, geophonic or anthrophonic).
#'
#' \emph{Power minus noise (PMN)}:
#' The difference between the maximum decibel value in
#' each frequency bin and the corresponding BGN decibel value.
#'
#' \emph{Acoustic cover (CVR)}:
#' The fraction of active elements in each noise-reduced
#' frequency bin where the amplitude exceeds a 3-dB threshold.
#'
#' \emph{Number of events (EVN)}:
#' The number of times the decibel value in a noise-reduced
#' frequency bin crosses the 3-dB threshold from lower to higher values.
#'
#' \emph{Temporal entropy (ENT)}:
#' A measure of acoustic energy concentration in each noise-reduced
#' frequency bin.
#'
#' \emph{Acoustic Complexity Index (ACI)}:
#' A measure quantifying the variability in
#' intensity values in each noise-reduced frequency bin. It is widely used as a
#' measure of biophony in recordings, however remains highly sensitive to
#' non-biological sources of sound.
#'
#' \emph{Oscillation Index (OSC)}:
#' ...
#'
#' \emph{Spectral Peak Tracks (SPT)}:
#' A measure of the presence of spectral peak
#' tracks in a noise-reduced frequency bin.
#'
#' \emph{Ridge indices (RHZ, RVT, RPS, RNS)}:
#' A set of indices based on the presence of formants
#' in the harmonic structure of many animal vocalizations, calculated in the
#' four directions of the ridge slope (horizontal (RHZ), vertical (RVT),
#' upward slope (RPS), downward slope (RNS)). Formants in the mid-band are
#' typically due to birdsong, whereas vertical formants are typical for
#' non-biological sounds such as rain drops and electrical clicks.
#'
#' For index computation this function calls on the external software
#' 'AnalysisPrograms.exe' by the QUT Ecoacoustics Research Group
#' (\url{https://github.com/QutEcoacoustics/audio-analysis}).Make sure the
#' software is installed prior to commencing the analysis. Additionally, the
#' software requires sound files to be named according to certain standards
#' (\url{https://ap.qut.ecoacoustics.info/basics/dates.html}).
#'
#' \strong{Software citation}:
#' Michael Towsey, Anthony Truskinger, Mark Cottman-Fields, & Paul Roe.
#' (2018, March 5). Ecoacoustics Audio Analysis Software v18.03.0.41
#' (Version v18.03.0.41). Zenodo. http://doi.org/10.5281/zenodo.1188744
#'
#' @return Creates a directory with '.csv' files for the aforementioned
#' set of spectral and summary indices. The directory is created in the same
#' place as the original sound file.
#'
#' @export
#'
index_calc <- function(fileloc,
                       progloc,
                       samplerate = 44100,
                       window = 256,
                       parallel = FALSE) {

  # 1. Check if function input meets expectations

  # 1.1. fileloc is an existing directory

  test_1 <- function(x){
    assertthat::is.dir(x)
    assertthat::is.readable(x)
  }

  assertthat::on_failure(test_1) <- function(call, env) {

    paste0(deparse(call$x), " is not a readable directory. If you're using a windows device, make sure to change backslashes to forwards slashes in the path.")

  }

  assertthat::assert_that(test_1(fileloc))

  # 2. Create output directory, and list files in fileloc + checking

  base_output_directory <- paste0(fileloc, "/", window)

  files <- list.files(fileloc,
                      pattern = "*.wav|*.WAV|*.mp3|
                      *.ogg|*.flac|*.wv|*.webm|*.wma",
                      full.names = TRUE
  )

  # 2.1. check if 'files' is a character vector, not empty, without NAs

  test_4_1 <- function(x){
    is.vector(x, mode = "character")
  }

  assertthat::on_failure(test_4_1) <- function(call, env){

    paste0(deparse(call$x), " is not a character vector containing the path to each file. Check if the correct file location name was supplied, and that files have the correct extension (consult package information for accepted file types)")

  }

  assertthat::assert_that(test_4_1(files))

  test_4_2 <- function(x){
    assertthat::not_empty(x)
  }

  assertthat::on_failure(test_4_2) <- function(call, env){

    paste0(deparse(call$x), " is an empty character vector, files were not succesfully read by list.files(). Check if the correct file location was supplied, and that files have the correct extention (consult package information for accepted file types")

  }

  assertthat::assert_that(test_4_2(files))

  test_4_3 <- function(x){
    assertthat::noNA(x)
  }

  assertthat::on_failure(test_4_3) <- function(call, env){

    paste0(deparse(call$x), " contains NAs. Check if the correct file location was supplied, and that files have the correct extention (consult package information for accepted file types")

  }

  assertthat::assert_that(test_4_3(files))

  #3. Edit the configuration file

  index_config(progloc, samplerate = samplerate, window = window)

  #4. Calculate acoustic indices

  if (parallel == FALSE){

    for (i in 1:length(files)) {

      # Alert start

      message("Processing ", files[i])

      # get just the name of the file
      file_name <- basename(files[i])

      # make a folder for results
      output_directory <- normalizePath(
        file.path(
          base_output_directory, file_name
        )
      )
      dir.create(output_directory, recursive = TRUE)

      # prepare command
      command <- sprintf(
        'audio2csv "%s" "%s" "%s" -l 1 -p ',
        files[i],
        paste0(
          progloc,
          "/ConfigFiles/Towsey.Acoustic.Custom.yml"
        ),
        output_directory
      )

      # finally, execute the command
      system2(paste0(progloc, "/AnalysisPrograms.exe"), command)

    }
  }

  else{

    if (parallel == TRUE){

      no_cores <- detectCores() - 1
      cl <- makeCluster(no_cores)

      parallel::parLapply(cl, files, function(x) {

        # Alert start

        message("Processing ", x)

        # get just the name of the file
        file_name <- basename(x)

        # make a folder for results
        output_directory <- normalizePath(
          file.path(
            base_output_directory, file_name
          )
        )
        dir.create(output_directory, recursive = TRUE)

        # prepare command
        command <- sprintf(
          'audio2csv "%s" "%s" "%s" -l 1 -p ',
          x,
          paste0(
            progloc,
            "/ConfigFiles/Towsey.Acoustic.Custom.yml"
          ),
          output_directory
        )

        # finally, execute the command
        system2(paste0(progloc, "/AnalysisPrograms.exe"), command)

      })

    }
  }

  # 4.1. Check if the output folder was successfully created

  test_5 <- function(x){

    assertthat::is.dir(x)

  }

  assertthat::on_failure(test_4_3) <- function(call, env){

    paste0(deparse(call$x), " directory does not exist, no output folder was created.")

  }

  assertthat::assert_that(test_5(paste0(fileloc, "/", window)))

}

# Function for importing and merging index csv files

#' Import and Merge Acoustic Index '.csv' Files
#'
#' @description For an acoustic index of choice, imports all
#' spectral index '.csv' files produced by the \code{\link{index_calc}}
#' function, and merges them into a time-by-frequency data frame.
#'
#' @param fileloc The full-length path to the directory where the
#' output directory of the \code{\link{index_calc}} function are located.
#'
#' @param samplerate The sampling rate specified in the \code{\link{index_calc}}
#' function.
#'
#' @param window The window length specified in the \code{\link{index_calc}}
#' function.
#'
#' @param index The acoustic index of interest. Options are
#' "BGN", "PMN", "CVR", "EVN", "ENT", "ACI", "OSC", "SPT", "RHZ",
#' "RVT", "RPS" and "RNG". For a brief description of indices, consult
#' the \code{\link{index_calc}} documentation. For the "BGN" index, the minimum index
#'  value was added to all data frame values to obtain a data frame of positive
#'  values while retaining the relationship between individual OSUs
#'
#' @param date The first day of the recording period. Used
#'  for managing time-objects in R.
#'  Formatted as "YYYY-mm-dd".
#'
#' @param lat The latitude of the site at which the sound files were
#' collected. Coordinates should be specified in decimal degrees as a
#'  numerical variable.
#'
#' @param lon The longitude of the site at which the sound files were
#'  collected. Coordinates should be specified in decimal degrees as a
#'   numerical variable.
#'
#' @param twilight A character string of the twilight
#' method to be used for sunrise and sunset determination.
#' Options can be found in the
#'  \code{\link[photobiology]{day_night}} documentation.
#'
#' @return Returns a time-by-frequency dataframe of acoustic index values
#' for all files in the recording period.
#'
#' @export
#'
merge_csv <- function(fileloc,
                      samplerate,
                      window,
                      index,
                      date,
                      lat,
                      lon,
                      twilight = "sunlight") {

  #0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(fileloc))
  assertthat::assert_that(test_0(samplerate))
  assertthat::assert_that(test_0(window))
  assertthat::assert_that(test_0(index))
  assertthat::assert_that(test_0(date))
  assertthat::assert_that(test_0(lat))
  assertthat::assert_that(test_0(lon))

  # 1. Check if function inputs meet expectations

  # 1.1. fileloc is an existing, writable directory

  test_1 <- function(x){
    assertthat::is.dir(x)
  }

  assertthat::assert_that(test_1(fileloc))

  test_2 <- function(x){
    assertthat::is.readable(x)
  }

  assertthat::on_failure(test_2) <- function(call, env) {

    paste0(deparse(call$x), " is not a readable directory. Please change the permissions and try again.")

  }

  assertthat::assert_that(test_2(fileloc))

  # 1.2. samplerate is a single, positive integer

  test_3 <- function(x){
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is not a single, positive integer. Consult the package documentation for more information on the samplerate.")

  }

  assertthat::assert_that(test_3(samplerate))

  # 1.3. window is a single, positive integer

  test_4 <- function(x){
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_4) <- function(call, env){

    paste0(deparse(call$x), " is not a single, positive integer. Consult the package documentation for more information on the window.")

  }

  assertthat::assert_that(test_4(window))

  # 1.4. produce a warning message if window is not a power of two

  if(!as.integer(log(window, base=2)) == (log(window, base = 2))){
    cat("\n Chosen window size is not a power of two. This is a warning message, \n if your window size was chosen purposefully, proceed as planned by pressing Y. \n If you want to abort, press N.", "\n")
    Sys.sleep(0.000000000001)

    question1 <- readline("Would you like to proceed with the chosen window size? (Y/N)")

    if(regexpr(question1, 'y', ignore.case = TRUE) == 1){
    }
    else{

      if(regexpr(question1, 'n', ignore.case = TRUE) == 1){
        print("Index computation aborted.")
        Sys.sleep(0.0000000000000000000000000001)
        stop()
      } else {
        print("The option you have chosen is not valid - index computation aborted")

        Sys.sleep(0.00000000000000000000000000000001)
        stop()
      }
    }

  }

  # 1.5. check if specified index is one of available options

  test_5 <- function(x){
    assertthat::is.string(x) &
      x %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
               "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG")
  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available spectral acoustic indices. Please consult package documentation for available options. Pay attention to capital letters and the presence of excess spaces.")

  }

  assertthat::assert_that(test_5(index))

  # 1.6. Check if the supplied date argument is in the correct format

  test_6 <- function(x) {
    assertthat::is.string(x)
  }

  test_7 <- function(x) {
    formatted = try(as.Date(x, "%Y-%m-%d"), silent = TRUE)
    is_date = as.character(formatted) == x & !is.na(formatted)
    return(is_date)
  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.")

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.")

  }

  assertthat::assert_that(test_6(date))
  assertthat::assert_that(test_7(date))

  # 1.7. Check if latitude and longitude are specified in decimal degrees and
  # exist on Earth

  test_8 <- function(x){

    is.numeric(x) &
      x >= -90 &
      x <= 90

  }

  test_9 <- function(x){

    is.numeric(x) &
      x >= -180 &
      x <= 180

  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_8(lat))
  assertthat::assert_that(test_9(lon))

  # 1.8. Check if the twilight argument has the correct format and is one of the
  # available options

  # 1.11. Check if the supplied twilight argument is one
  # of the available options

  test_10 <- function(x){

    (assertthat::is.string(x) &
       x %in% c("none","rim","refraction","sunlight","civil",
                "nautical","astronomical")) |
      (is.vector(x, mode="double") & (length(x) == 1 | length(x) ==2))

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.")

  }

  assertthat::assert_that(test_10(twilight))


  # 2. Get a list of the acoustic index '.csv' files in the specified file location

  setwd(paste0(fileloc, "/", window))
  folders <- getwd()

  filenames <- list.files(folders,
                          pattern = paste0("\\__Towsey.Acoustic.",
                                           index,
                                           ".csv$"),
                          recursive = TRUE)

  # 3. Merge csv files into a dataframe

  merged_df <- Reduce(rbind, lapply(filenames, utils::read.csv))


  # 4. Perform some manipulations on the merged_df

  merged_df <- as.matrix(merged_df)
  merged_df <- t(merged_df)
  merged_df <- as.data.frame(merged_df)
  merged_df <- merged_df[c(2:nrow(merged_df)), ]

  # 5. Give the df rownames

  if (index == "OSC") {
    frequency_bins <- as.integer(
      seq(
        from = (samplerate / 2) / (window),
        to = samplerate / 2,
        by = (((samplerate / 2) / (window)))))
  }

  else {
    frequency_bins <- frequency_bins_128 <- as.integer(
      seq(
        from = (samplerate/window),
        to = samplerate / 2,
        by = (samplerate/window)))
  }

  row.names(merged_df) <- as.integer(frequency_bins)

  # 6. Give the df colnames

  tz <- lutz::tz_lookup_coords(lat = lat, lon = lon, method = "accurate")
  colnames(merged_df) <- hms::as_hms(
    as.POSIXct(
      substr(filenames,
             nchar(filenames) - 40,
             nchar(filenames) - 26),
      format = "%Y%m%d_%H%M%S",
      tz = tz))

  merged_df <- merged_df[seq(dim(merged_df)[1], 1), ]

  if(index=="BGN"){

    merged_df <- merged_df + abs(min(merged_df))

  }

  else{}

  # Create some more useful metadata elements for the soundscape object

  day <- as.POSIXct(
    strptime(
      paste(date,
            "00:00:00",
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))

  points <- data.frame(lon = lon, lat = lat)

  suntimes <- photobiology::day_night(
    date = date,
    tz= tz,
    geocode = points,
    twilight = twilight,
    unit.out = "datetime")

  sunrise <- as.POSIXct(suntimes$sunrise,
                        tz = tz,
                        format = "%Y-%m-%d %H:%M:%S")
  sunset <- as.POSIXct(suntimes$sunset,
                       tz = tz,
                       format = "%Y-%m-%d %H:%M:%S")

  merged_soundscape <- new("soundscape",
                           fileloc = fileloc,
                           index = index,
                           samplerate = samplerate,
                           window = window,
                           first_day = day,
                           lat = lat,
                           lon = lon,
                           tz = tz,
                           sunrise = sunrise,
                           sunset = sunset,
                           merged_df = merged_df)


  merged_soundscape

}


# Functions for applying a binarization algorithm to the
# merged dataframe

#' Calculate the mode
#'
#' @param df The merged_soundscape data frame produced by
#' \code{\link{merge_csv}}.
#' @return Returns the acoustic index value which appears most often
#' @details Function for internal use by \code{\link{threshold_df}}
#'

get_mode <- function(df) {

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(df))



  # 1. Check if input variable in the right format

  test_1 <- function(x){

    (is.data.frame(x) & limma::isNumeric(x))
  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " is not a numeric dataframe. Please supply a valid argument to the function")

  }

  assertthat::assert_that(test_1(df))

  # Get the mode of the dataframe or vector

  uniqv <- unique(unlist(df))
  uniqv[which.max(tabulate(match(unlist(df), uniqv)))]
}

#' Determine Binarization Threshold
#'
#' @description Determines the threshold for binarization of the
#' time-frequency dataframe of index values. Several binarization
#' algorithms are available, either based on image thresholding
#' algorithms from \code{\link[autothresholdr]{auto_thresh}}, or
#'  calculation of the mode.
#'
#' @param df The merged_soundscape data frame produced by
#' \code{\link{merge_csv}}.
#'
#' @param method The algorithm used to determine the threshold.
#'  Options are "IJDefault", "Huang", "Huang2", "Intermodes",
#'  "IsoData", "Li", "MaxEntropy", "Mean", "MinErrorI", "Minimum",
#'  "Moments", "Otsu", "Percentile", "RenyiEntropy", "Shanbhag",
#'  "Triangle", "Yen", and "Mode".
#'  Consult \url{http://imagej.net/Auto_Threshold} for more
#'  information on algorithm methodologies.
#'
#' @details Function for internal use by \code{\link{binarize_df}},
#' however can also be called on by the user manually.
#'
#' @return Returns a numeric threshold value for subsequent
#' binarization.
#'
#' @export
#'
threshold_df <- function(df, method) {

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(df))
  assertthat::assert_that(test_0(method))


  # 1. Check if function input meets expectations

  #1.1. The supplied dataframe is a dataframe, is not empty, and does not contain NAs

  test_1 <- function(x){

    is.data.frame(x)

  }

  test_2 <- function(x){

    assertthat::not_empty(x)

  }

  test_3 <- function(x){

    assertthat::noNA(x)

  }

  test_4 <- function(x){

    limma::isNumeric(x)

  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " is not a data frame. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " is an empty dataframe. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " contains NA values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::on_failure(test_4) <- function(call, env){

    paste0(deparse(call$x), " contains non-numeric values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::assert_that(test_1(df))
  assertthat::assert_that(test_2(df))
  assertthat::assert_that(test_3(df))
  assertthat::assert_that(test_4(df))

  # 1.2. Check if the specified method is one of the available options

  test_2 <- function(x){
    assertthat::is.string(x) &
      x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li",
               "MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu",
               "Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode")
  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available binarization methods. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_2(method))

  # 2. Calculate the threshold value

  if (method == "IJDefault" |
      method == "Huang" |
      method == "Huang2" |
      method == "Intermodes" |
      method == "IsoData" |
      method == "Li" |
      method == "MaxEntropy" |
      method == "Mean" |
      method == "MinErrorI" |
      method == "Minimum" |
      method == "Moments" |
      method == "Otsu" |
      method == "Percentile" |
      method == "RenyiEntropy" |
      method == "Shanbhag" |
      method == "Triangle" |
      method == "Yen") {

    df2 <- (as.integer(as.matrix(df) * 100))
    threshold <- autothresholdr::auto_thresh(int_arr = df2, method = method)
    threshold <- threshold / 100

    return(threshold)

  }

  else {

    if (method == "Mode" | method == "mode") {

      mode <- get_mode(df)

      return(mode)

    }
  }
}

#' Binarize Dataframe
#'
#' @description Separates acoustically active time-frequency bins
#'  (active=1) from background values (inactive=0) through the
#'  application of a binarization algorithm. Several binarization
#'  algorithms are available, either based on image thresholding
#'  using \code{\link[autothresholdr]{auto_thresh}}, or subtraction
#'  of the mode.
#'
#' @param merged_soundscape The merged soundscape object produced by
#' \code{\link{merge_csv}}.
#'
#' @param method The algorithm used to determine the threshold.
#'  Options are "IJDefault","Huang", "Huang2", "Intermodes",
#'  "IsoData", "Li", "MaxEntropy", "Mean", "MinErrorI", "Minimum",
#'  "Moments", "Otsu", "Percentile", "RenyiEntropy", "Shanbhag",
#'  "Triangle", "Yen", and "Mode". To specify a custom threshold,
#'  use method="Custom" in combination with the value argument.
#'  Consult \url{http://imagej.net/Auto_Threshold} for more
#'  information on algorithm methodologies.
#'
#' @param value Optional argument used to set a custom threshold
#' value for binarization - used in combination with method="Custom".
#'
#' @return Returns a binary time-frequency dataframe of acoustic
#'  activity (active=1, inactive=0) for a set acoustic index.
#' @export
#'
binarize_df <- function(merged_soundscape,
                        method,
                        value = NULL){

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  test_1 <- function(x, y){

    !(method == "custom" & (missing(x) | is.null(x)))

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. If you set method to 'custom', please supply a value argument. Consult package documentation for options.")

  }

  assertthat::assert_that(test_0(merged_soundscape))
  assertthat::assert_that(test_0(method))
  assertthat::assert_that(test_1(value))

  # 1. Check if function input meets expectations

  # 1.1. The supplied merged_soundscape argument is an S4-object of the type
  # 'soundscape', and is not empty.

  test_2 <- function(x){

    isS4(x) &
      assertthat::are_equal(class(x)[1], "soundscape")

  }

  test_3 <- function(x){

    assertthat::not_empty(x)

  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " is not an S4-object of the type 'soundscape'. Please supply the merged_soundscape object produced by the merge_csv() function. Consult the package documentation for further information.")

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is an empty S4-object of the type 'soundscape'. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function.")

  }

  assertthat::assert_that(test_2(merged_soundscape))
  assertthat::assert_that(test_3(merged_soundscape))

  # 1.2. The merged_soundscape elements are in the expected format

  # 1.2.1. The first_day argument cannot be wrong (S4 property)

  # 1.2.2. The lat and lon argument

  test_5 <- function(x){

    is.numeric(x) &
      x >= -90 &
      x <= 90

  }

  test_6 <- function(x){

    is.numeric(x) &
      x >= -180 &
      x <= 180

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_5(merged_soundscape@lat))
  assertthat::assert_that(test_6(merged_soundscape@lon))

  # 1.2.3. The time zone argument

  test_7 <- function(x){

    assertthat::is.string(x) & (x %in% (OlsonNames()))

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")

  }

  assertthat::assert_that(test_7(merged_soundscape@tz))

  # 1.2.4. The sunrise and sunset arguments cannot be wrong (s4 property)

  # 1.2.5. The fileloc argument
  #
  #   test_8 <- function(x){
  #
  #     assertthat::is.dir(x) & assertthat::is.readable(x)
  #
  #   }

  # assertthat::assert_that(test_8(merged_soundscape@fileloc))

  # 1.2.6. The index argument

  test_9 <- function(x){

    assertthat::is.string(x) & (x %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
                                         "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"))

  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available index options. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")

  }

  assertthat::assert_that(test_9(merged_soundscape@index))

  # 1.2.7. The samplerate and window arguments

  test_10 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the samplerate and window arguments.")

  }

  assertthat::assert_that(test_10(merged_soundscape@samplerate))
  assertthat::assert_that(test_10(merged_soundscape@window))

  # 1.2.8. The post-binarization arguments are NA

  test_11 <- function(x){

    is.na(x)

  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not NA. Did you supply a post-binarization or post-aggregation soundscape to the binarize_df() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_11(merged_soundscape@binarization_method))
  assertthat::assert_that(test_11(merged_soundscape@threshold))
  assertthat::assert_that(test_11(merged_soundscape@output))

  # 1.2.9. The merged_df argument

  test_12 <- function(x){

    is.data.frame(x) &
      assertthat::not_empty(x) &
      assertthat::noNA(x) &
      limma::isNumeric(x)

  }

  test_13 <- function(x){

    (abs(as.numeric(rownames(x)[1]))+
       abs(as.numeric(rownames(x)[2])))>3 &
      min(as.numeric(rownames(x))) >= 0 &
      max(as.numeric(rownames(x)))<= merged_soundscape@samplerate/2

  }

  test_14 <- function(x){

    formatted <-  try(
      as.POSIXct(
        paste0(substr(merged_soundscape@first_day, 1, 12)," ", colnames(x)),
        tz = merged_soundscape@tz,
        format="%Y-%m-%d %H:%M:%S"),
      silent = TRUE)

    !any(sapply(formatted, function(y) is.na(y)))

  }


  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function.")

  }

  assertthat::on_failure(test_13) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::on_failure(test_14) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::assert_that(test_12(merged_soundscape@merged_df))
  assertthat::assert_that(test_13(merged_soundscape@merged_df))
  assertthat::assert_that(test_14(merged_soundscape@merged_df))

  # 1.2.10. The binarized_df and aggregate_df arguments are missing

  test_15 <- function(x){

    assertthat::are_equal(x[1, 1], "missing")

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " is not a missing data frame. Did you supply a post-binarization or post-aggregation merged_soundscape to the binarize_df() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_15(merged_soundscape@binarized_df))
  assertthat::assert_that(test_15(merged_soundscape@aggregated_df))

  test_16 <- function(x){

    all(sapply(x, function(x) is.na(x)))

  }

  assertthat::on_failure(test_16) <- function(call, env){

    paste0(deparse(call$x), " is not a list of NAs. Did you supply a post-binarization or post-aggregation merged_soundscape to the binarize_df() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_16(merged_soundscape@aggregated_df_per_time))
  assertthat::assert_that(test_16(merged_soundscape@effort_per_time))


  # 1.3. Check if the specified method is one of the available options

  test_17 <- function(x){
    assertthat::is.string(x)
  }

  test_18 <- function(x){
    x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li","MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu","Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode", "custom")
  }

  assertthat::on_failure(test_17) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the binarization methods as a character string. Consult package documentation for available method options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_18) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available binarization method options. Please consult package documentation for available  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_17(method))
  assertthat::assert_that(test_18(method))

  # 1.3. Check that, if value is supplied, it is a single number

  test_19 <- function(x){

    is.null(x) |
      is.numeric(x) & length(x) == 1

  }

  assertthat::on_failure(test_19) <- function(call, env){

    paste0(deparse(call$x), " input is not in the right format. To choose a custom binarization threshold, supply a single numeric value.")

  }

  assertthat::assert_that(test_19(value))

  # 2. Calculate the binarization threshold

  if (method == "IJDefault" |
      method == "Huang" |
      method == "Huang2" |
      method == "Intermodes" |
      method == "IsoData" |
      method == "Li" |
      method == "MaxEntropy" |
      method == "Mean" |
      method == "MinErrorI" |
      method == "Minimum" |
      method == "Moments" |
      method == "Otsu" |
      method == "Percentile" |
      method == "RenyiEntropy" |
      method == "Shanbhag" |
      method == "Triangle" |
      method == "Yen" |
      method == "Mode" |
      method == "mode") {
    threshold <- threshold_df(df = merged_soundscape@merged_df, method = method)
  }

  else {
    if (method == "custom") {
      threshold <- value
    }
  }

  # 3. Perform dataframe binarization

  thresh_df <- as.data.frame(ifelse(merged_soundscape@merged_df > (threshold), 1, 0))
  colnames(thresh_df) <- colnames(merged_soundscape@merged_df)
  rownames(thresh_df) <- rownames(merged_soundscape@merged_df)

  binarized_soundscape <- new("soundscape",
                              first_day = merged_soundscape@first_day,
                              lat = merged_soundscape@lat,
                              lon = merged_soundscape@lon,
                              tz = merged_soundscape@tz,
                              sunrise = merged_soundscape@sunrise,
                              sunset = merged_soundscape@sunset,
                              fileloc = merged_soundscape@fileloc,
                              index = merged_soundscape@index,
                              samplerate = merged_soundscape@samplerate,
                              window = merged_soundscape@window,
                              binarization_method = method,
                              threshold = as.numeric(threshold),
                              merged_df = merged_soundscape@merged_df,
                              binarized_df = thresh_df)

  binarized_soundscape

}


#' Check which binarization algorithm works for a set index
#'
#' @description Plots the merged acoustic index dataframe
#'  before and after the binarization step using a
#'  user-specified binarization algorithm. Facilitates the
#'  rapid visual comparison of binarization performance using
#'  different algorithms.
#'
#' @param merged_soundscape The merged_soundscape object produced by
#' \code{\link{merge_csv}}.
#'
#' @param method The algorithm used to determine the
#' threshold. Options are "IJDefault","Huang", "Huang2",
#'  "Intermodes", "IsoData", "Li", "MaxEntropy", "Mean",
#'  "MinErrorI", "Minimum", "Moments", "Otsu", "Percentile",
#'  "RenyiEntropy", "Shanbhag", "Triangle", "Yen", and
#'  "Mode". To specify a custom threshold, use
#'  method="Custom" in combination with the value argument.
#'  Consult \url{http://imagej.net/Auto_Threshold} for more
#'  information on algorithm methodologies.
#'
#' @param value Optional argument used to set a custom
#'  threshold value for binarization - used in combination
#'   with method="Custom".
#'
#' @return A plot with before and after binarization to
#'  facilitate visual comparison of binarization algorithm
#'   performance.
#'
#' @export
#'
check_thresh <- function(merged_soundscape,
                         method,
                         value = NULL){

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  test_1 <- function(x, y){

    !(method == "custom" & (missing(x) | is.null(x)))

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. If you set method to 'custom', please supply a value argument. Consult package documentation for options.")

  }

  assertthat::assert_that(test_0(merged_soundscape))
  assertthat::assert_that(test_0(method))
  assertthat::assert_that(test_1(value))

  # 1. Check if function input meets expectations

  # 1.1. The supplied merged_soundscape argument is an S4-object of the type
  # 'soundscape', and is not empty.

  test_2 <- function(x){

    isS4(x) &
      assertthat::are_equal(class(x)[1], "soundscape")

  }

  test_3 <- function(x){

    assertthat::not_empty(x)

  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " is not an S4-object of the type 'soundscape'. Please supply the merged_soundscape object produced by the merge_csv() function. Consult the package documentation for further information.")

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is an empty S4-object of the type 'soundscape'. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function.")

  }

  assertthat::assert_that(test_2(merged_soundscape))
  assertthat::assert_that(test_3(merged_soundscape))

  # 1.2. The merged_soundscape elements are in the expected format

  # 1.2.1. The first_day argument cannot be wrong (S4 property)

  # 1.2.2. The lat and lon argument

  test_5 <- function(x){

    is.numeric(x) &
      x >= -90 &
      x <= 90

  }

  test_6 <- function(x){

    is.numeric(x) &
      x >= -180 &
      x <= 180

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_5(merged_soundscape@lat))
  assertthat::assert_that(test_6(merged_soundscape@lon))

  # 1.2.3. The time zone argument

  test_7 <- function(x){

    assertthat::is.string(x) & (x %in% (OlsonNames()))

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")

  }

  assertthat::assert_that(test_7(merged_soundscape@tz))

  # 1.2.4. The sunrise and sunset arguments cannot be wrong (s4 property)

  # 1.2.5. The fileloc argument

  # test_8 <- function(x){
  #
  #   assertthat::is.dir(x) & assertthat::is.readable(x)
  #
  # }
  #
  # assertthat::assert_that(test_8(merged_soundscape@fileloc))

  # 1.2.6. The index argument

  test_9 <- function(x){

    assertthat::is.string(x) & (x %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
                                         "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"))

  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available index options. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")

  }

  assertthat::assert_that(test_9(merged_soundscape@index))

  # 1.2.7. The samplerate and window arguments

  test_10 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the samplerate and window arguments.")

  }

  assertthat::assert_that(test_10(merged_soundscape@samplerate))
  assertthat::assert_that(test_10(merged_soundscape@window))

  # 1.2.8. The post-binarization arguments are NA

  test_11 <- function(x){

    is.na(x)

  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not NA. Did you supply a post-binarization or post-aggregation soundscape to the check_thresh() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_11(merged_soundscape@binarization_method))
  assertthat::assert_that(test_11(merged_soundscape@threshold))
  assertthat::assert_that(test_11(merged_soundscape@output))

  # 1.2.9. The merged_df argument

  test_12 <- function(x){

    is.data.frame(x) &
      assertthat::not_empty(x) &
      assertthat::noNA(x) &
      limma::isNumeric(x)

  }

  test_13 <- function(x){

    (abs(as.numeric(rownames(x)[1]))+
       abs(as.numeric(rownames(x)[2])))>3 &
      min(as.numeric(rownames(x))) >= 0 &
      max(as.numeric(rownames(x)))<= merged_soundscape@samplerate/2

  }

  test_14 <- function(x){

    formatted <-  try(
      as.POSIXct(
        paste0(substr(merged_soundscape@first_day, 1, 12)," ", colnames(x)),
        tz = merged_soundscape@tz,
        format="%Y-%m-%d %H:%M:%S"),
      silent = TRUE)

    !any(sapply(formatted, function(y) is.na(y)))

  }


  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function.")

  }

  assertthat::on_failure(test_13) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::on_failure(test_14) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::assert_that(test_12(merged_soundscape@merged_df))
  assertthat::assert_that(test_13(merged_soundscape@merged_df))
  assertthat::assert_that(test_14(merged_soundscape@merged_df))

  # 1.2.10. The binarized_df and aggregate_df arguments are missing

  test_15 <- function(x){

    assertthat::are_equal(x[1, 1], "missing")

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " is not a missing data frame. Did you supply a post-binarization or post-aggregation merged_soundscape to the check_thresh() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_15(merged_soundscape@binarized_df))
  assertthat::assert_that(test_15(merged_soundscape@aggregated_df))

  test_16 <- function(x){

    all(sapply(x, function(x) is.na(x)))

  }

  assertthat::on_failure(test_16) <- function(call, env){

    paste0(deparse(call$x), " is not a list of NAs. Did you supply a post-binarization or post-aggregation merged_soundscape to the check_thresh() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_16(merged_soundscape@aggregated_df_per_time))
  assertthat::assert_that(test_16(merged_soundscape@effort_per_time))


  # 1.3. Check if the specified method is one of the available options

  test_17 <- function(x){
    assertthat::is.string(x)
  }

  test_18 <- function(x){
    x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li","MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu","Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode", "custom")
  }

  assertthat::on_failure(test_17) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the binarization methods as a character string. Consult package documentation for available method options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_18) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available binarization method options. Please consult package documentation for available  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_17(method))
  assertthat::assert_that(test_18(method))

  # 1.4. Check that, if value is supplied, it is a single number

  test_19 <- function(x){

    is.null(x) |
      is.numeric(x) & length(x) == 1

  }

  assertthat::on_failure(test_19) <- function(call, env){

    paste0(deparse(call$x), " input is not in the right format. To choose a custom binarization threshold, supply a single numeric value.")

  }

  assertthat::assert_that(test_19(value))

  # 2. Get the before and after dataframe in the right format

  # 2.1. Make function to lengthen data frames for ggplot

  lengthen_2 <- function(df){
    tz <- df@tz
    df_2 <- df@merged_df
    df_2$frequency=as.integer(rownames(df_2))
    melt_df=reshape2::melt(df_2, id.vars="frequency")
    colnames(melt_df)=c("frequency", "time", "value")
    melt_df$frequency=as.numeric(as.character(melt_df$frequency))
    melt_df$time=as.POSIXct(strptime(paste(melt_df$time),
                                     format= "%Y-%m-%d %H:%M",
                                     tz=tz))
    melt_df
  }

  if (method == "IJDefault" |
      method == "Huang" |
      method == "Huang2" |
      method == "Intermodes" |
      method == "IsoData" |
      method == "Li" |
      method == "MaxEntropy" |
      method == "Mean" |
      method == "MinErrorI" |
      method == "Minimum" |
      method == "Moments" |
      method == "Otsu" |
      method == "Percentile" |
      method == "RenyiEntropy" |
      method == "Shanbhag" |
      method == "Triangle" |
      method == "Yen" |
      method == "Mode" |
      method == "mode") {
    threshold <- threshold_df(df = merged_soundscape@merged_df, method = method)
  }

  else {
    if (method == "custom") {
      threshold <- value
    }
  }

  df_after <- as.data.frame(
    ifelse(merged_soundscape@merged_df > (threshold), 1, 0))

  colnames(df_after) <- colnames(merged_soundscape@merged_df)

  rownames(df_after) <- rownames(merged_soundscape@merged_df)

  # 3. Figure out the time difference between two
  #consecutive recordings in the df_after df

  time_1 <- as.POSIXct(
    strptime(
      paste(substr(merged_soundscape@first_day, 1, 12),
            colnames(df_after)[1],
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz = merged_soundscape@tz))

  time_2 <- as.POSIXct(
    strptime(
      paste(substr(merged_soundscape@first_day, 1, 12),
            colnames(df_after)[2],
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz = merged_soundscape@tz))

  new_colnames <- seq.POSIXt(
    from = as.POSIXct(
      strptime(
        paste(
          substr(merged_soundscape@first_day, 1, 12),
          colnames(df_after)[1],
          sep=" "),
        format= "%Y-%m-%d %H:%M:%S",
        tz = merged_soundscape@tz)),
    to = as.POSIXct(
      strptime(
        paste(substr(merged_soundscape@first_day, 1, 12),
              colnames(merged_soundscape@merged_df)[1],
              sep=" "),
        format= "%Y-%m-%d %H:%M:%S",
        tz = merged_soundscape@tz)) + (ncol(merged_soundscape@merged_df)*(time_2-time_1)),
    by = (time_2 - time_1)
  )[1:ncol(merged_soundscape@merged_df)]

  new_colnames

  colnames(df_after) <- new_colnames

  colnames(merged_soundscape@merged_df) <- new_colnames

  df_before <- lengthen_2(df = merged_soundscape)

  df_before

  # 3.1. Test if all the objects are in the right format

  # time_1 and time_2

  test_13 <- function(x, y){

    lubridate::is.POSIXct(x) &
      lubridate::is.POSIXct(y)

  }

  assertthat::assert_that(test_13(x=time_1, y=time_2))

  # new_colnames

  test_14 <- function(x){

    !any(!sapply(x, FUN = lubridate::is.POSIXct))

  }

  assertthat::assert_that(test_14(new_colnames))

  # 4. Lengthen the df_after data frame

  lengthen_3 <- function(df, date, lat, lon){
    tz <- lutz::tz_lookup_coords(lat = lat, lon = lon, method = "accurate")
    df_2 <- df
    df_2$frequency=as.integer(rownames(df_2))
    melt_df=reshape2::melt(df_2, id.vars="frequency")
    colnames(melt_df)=c("frequency", "time", "value")
    melt_df$frequency=as.numeric(as.character(melt_df$frequency))
    melt_df$time=as.POSIXct(strptime(paste(melt_df$time),
                                     format= "%Y-%m-%d %H:%M",
                                     tz=tz))
    melt_df
  }

  df_after <- lengthen_3(df = df_after, date = paste0(substr(merged_soundscape@first_day, 1, 12)), lat = merged_soundscape@lat, lon = merged_soundscape@lon)

  df_after

  # 4.1. Check if the df_before and df_after lengthened
  # data frames are in the right format

  test_15 <- function(x){

    is.data.frame(x)

  }

  test_16 <- function(x){

    assertthat::not_empty(x)

  }

  test_17 <- function(x){

    assertthat::noNA(x)

  }

  test_18 <- function(x){

    nrow(x) == (nrow(merged_soundscape@merged_df)*ncol(merged_soundscape@merged_df))

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " is not a data frame. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function")

  }

  assertthat::assert_that(test_15(df_before))
  assertthat::assert_that(test_16(df_before))
  assertthat::assert_that(test_17(df_before))
  assertthat::assert_that(test_18(df_before))

  assertthat::assert_that(test_15(df_after))
  assertthat::assert_that(test_16(df_after))
  assertthat::assert_that(test_17(df_after))
  assertthat::assert_that(test_18(df_after))


  # 4. Prepare other objects for ggplot

  timeinterval <- "12 hours"

  freqinterval <- (max(df_before$frequency)/10)

  mintime <- min(new_colnames)

  maxtime <- max(new_colnames)

  minfreq <- 0

  maxfreq <- max(df_before$frequency)

  # Make a plot

  plot_before <- suppressWarnings(

    ggplot2::ggplot(
      data = df_before,
      ggplot2::aes(x = time,
                   y = frequency,
                   fill = value)) +

      ggplot2::geom_tile() +

      viridis::scale_fill_viridis(
        option = "D",
        na.value="#383e42",
        direction = 1,
        guide = ggplot2::guide_legend(
          title.position = "top",
          title.vjust = 1,
          title.hjust = 0.5,
          nrow=1),
        breaks=round(
          seq(
            min(df_before$value),
            max(df_before$value),
            max(df_before$value)/10),
          digits = 2)) +


      ggplot2::scale_x_datetime(
        labels=scales::date_format("%H:%M", tz=merged_soundscape@tz),
        breaks = scales::date_breaks(timeinterval),
        expand = c(0,0),
        limits = c(mintime,maxtime)) +

      ggplot2::scale_y_continuous(
        limits = c(minfreq, (maxfreq+(maxfreq/10))),
        expand = c(0,0),
        breaks = seq(minfreq, maxfreq, freqinterval)) +
      ggplot2::labs(y="Frequency (Hz) \n", x="\nTime (hour of day)") +

      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black"),
        axis.text.y = ggplot2::element_text(color = "black",
                                            size = 10),
        axis.text.x = ggplot2::element_text(color = "black",
                                            size = 10,
                                            angle = 45,
                                            hjust=1.1,
                                            vjust=1),
        axis.title.y = ggplot2::element_text(),
        axis.title.x = ggplot2::element_text(),
        plot.margin = grid::unit(c(1,1,1,1),"cm"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = ggplot2::element_text(color = "black",
                                             size = 12,
                                             face = "bold")) +

      ggplot2::labs(fill="Index value")

    # ggplot2::annotate(geom= "label",
    #                   label = "BEFORE",
    #                   x = new_colnames[as.integer((length(new_colnames)/10)*9)],
    #                   y = (maxfreq - (maxfreq / 8)),
    #                   fill = "white",
    #                   color = "black",
    #                   fontface = 2)
  )

  plot_after <- suppressWarnings(

    ggplot2::ggplot(
      data = df_after,
      ggplot2::aes(x = time,
                   y = frequency,
                   fill = value)) +

      ggplot2::geom_tile() +

      viridis::scale_fill_viridis(
        option = "D",
        na.value="#383e42",
        direction = 1,
        guide = ggplot2::guide_legend(
          title.position = "top",
          title.vjust = 1,
          title.hjust = 0.5,
          nrow=1),
        breaks=seq(0, 1, 0.1)) +


      ggplot2::scale_x_datetime(
        labels=scales::date_format("%H:%M", tz=merged_soundscape@tz),
        breaks = scales::date_breaks(timeinterval),
        expand = c(0,0),
        limits = c(mintime,maxtime)) +

      ggplot2::scale_y_continuous(
        limits = c(minfreq, (maxfreq+(maxfreq/10))),
        expand = c(0,0),
        breaks = seq(minfreq, maxfreq, freqinterval)) +
      ggplot2::labs(y="Frequency (Hz) \n", x="\nTime (hour of day)") +

      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black"),
        axis.text.y = ggplot2::element_text(color = "black",
                                            size = 10),
        axis.text.x = ggplot2::element_text(color = "black",
                                            size = 10,
                                            angle = 45,
                                            hjust=1.1,
                                            vjust=1),
        axis.title.y = ggplot2::element_text(),
        axis.title.x = ggplot2::element_text(),
        plot.margin = grid::unit(c(1,1,1,1),"cm"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = ggplot2::element_text(color = "black",
                                             size = 12,
                                             face = "bold")) +

      ggplot2::labs(fill="Index value")

    # ggplot2::annotate(geom= "label",
    #                   label = "AFTER",
    #                   x = new_colnames[as.integer((length(new_colnames)/10)*9)],
    #                   y = (maxfreq - (maxfreq / 8)),
    #                   fill = "white",
    #                   color = "black",
    #                   fontface = 2)
  )

  library(patchwork)


  plot <- plot_before/plot_after

  suppressWarnings(print(plot))

}

#' Aggregate binarized dataframe
#'
#'@description Aggregates binarized spectral indices by time of day.
#' For each aggregate time period, the activity values (0 or 1)
#' are summed per frequency bin, and potentially divided by the number
#' of recordings for that time to get the proportion of
#' acoustically active recordings in each time-frequency bin
#' (the incidence frequency).
#'
#' @param binarized_soundscape The binarized soundscape object produced by
#'  \code{\link{binarize_df}}.

#' @param output Determines whether the function returns the raw
#' total number of detections (activity = 1) per time during the
#' recording period (output = "raw"), or the incidence frequency
#' (total number of detections / number of recordings for that time
#' - output = "incidence_freq).
#'
#' @return Returns a list containing three elements:
#' (i) aggregated_per_time: a list of soundscape samples
#' (the columns in the binarized_df function) grouped per
#'  unique time in the recording period; (
#'  ii) sampling_effort_per_time: a list indicating the
#'   sampling effort (number of soundscape samples) per
#'    unique time in the recording period;
#'    (iii) aggregated_df: an aggregated time-frequency
#'    data frame containing the number of acoustically
#'    active recordings in each bin for a set acoustic index.
#'
#' @export
#'
aggregate_df <- function(binarized_soundscape,
                         output = "incidence_freq"){

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(binarized_soundscape))

  # 1. Check if function input meets expectations

  # 1.1. The supplied binarized_soundscape argument is an S4-object of the type
  # 'soundscape', and is not empty.

  test_2 <- function(x){

    isS4(x) &
      assertthat::are_equal(class(x)[1], "soundscape")

  }

  test_3 <- function(x){

    assertthat::not_empty(x)

  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " is not an S4-object of the type 'soundscape'. Please supply the binarized_soundscape object produced by the binarize_df() function. Consult the package documentation for further information.")

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is an empty S4-object of the type 'soundscape'. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function.")

  }

  assertthat::assert_that(test_2(binarized_soundscape))
  assertthat::assert_that(test_3(binarized_soundscape))

  # 1.2. The binarized_soundscape elements are in the expected format

  # 1.2.1. The first_day argument cannot be wrong (S4 property)

  # 1.2.2. The lat and lon argument

  test_5 <- function(x){

    is.numeric(x) &
      x >= -90 &
      x <= 90

  }

  test_6 <- function(x){

    is.numeric(x) &
      x >= -180 &
      x <= 180

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_5(binarized_soundscape@lat))
  assertthat::assert_that(test_6(binarized_soundscape@lon))

  # 1.2.3. The time zone argument

  test_7 <- function(x){

    assertthat::is.string(x) & (x %in% (OlsonNames()))

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")

  }

  assertthat::assert_that(test_7(binarized_soundscape@tz))

  # 1.2.4. The sunrise and sunset arguments cannot be wrong (s4 property)

  # 1.2.5. The fileloc argument

  # test_8 <- function(x){
  #
  #   assertthat::is.dir(x) & assertthat::is.readable(x)
  #
  # }
  #
  # assertthat::assert_that(test_8(binarized_soundscape@fileloc))

  # 1.2.6. The index argument

  test_9 <- function(x){

    assertthat::is.string(x) & (x %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
                                         "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"))

  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available index options. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")

  }

  assertthat::assert_that(test_9(binarized_soundscape@index))

  # 1.2.7. The samplerate and window arguments

  test_10 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function, and pay special attention to the samplerate and window arguments.")

  }

  assertthat::assert_that(test_10(binarized_soundscape@samplerate))
  assertthat::assert_that(test_10(binarized_soundscape@window))

  # 1.2.8. The binarization_method argument

  test_11 <- function(x){
    assertthat::is.string(x) & (x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li","MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu","Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode", "custom"))
  }



  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_11(binarized_soundscape@binarization_method))

  # 1.2.9. The threshold argument

  test_12 <- function(x){

    all(length(x) == 1 &
          is.double(x) & !is.na(x))

  }

  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " is not a single numeric value. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function, and pay special attention to the value argument is you're supplying a custom threshold value.")

  }

  assertthat::assert_that(test_12(binarized_soundscape@threshold))

  # 1.2.10. The output argument

  test_14 <- function(x){

    is.na(x)

  }

  assertthat::on_failure(test_14) <- function(call, env){

    paste0(deparse(call$x), " is not NA. Did you supply a post-binarization or post-aggregation soundscape to the check_thresh() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_14(binarized_soundscape@output))

  # 1.2.11. The merged_df argument

  test_15 <- function(x){

    is.data.frame(x) &
      assertthat::not_empty(x) &
      assertthat::noNA(x) &
      limma::isNumeric(x)

  }

  test_16 <- function(x){

    (abs(as.numeric(rownames(x)[1]))+
       abs(as.numeric(rownames(x)[2])))>3 &
      min(as.numeric(rownames(x))) >= 0 &
      max(as.numeric(rownames(x)))<= binarized_soundscape@samplerate/2

  }

  test_17 <- function(x){

    formatted <-  try(
      as.POSIXct(
        paste0(substr(binarized_soundscape@first_day, 1, 12)," ", colnames(x)),
        tz = binarized_soundscape@tz,
        format="%Y-%m-%d %H:%M:%S"),
      silent = TRUE)

    !any(sapply(formatted, function(y) is.na(y)))

  }


  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function.")

  }

  assertthat::on_failure(test_16) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of binarize_df(). Make sure you're supplying the dataframe produced by the binarize_df() function.")

  }

  # assertthat::on_failure(test_17) <- function(call, env){
  #
  #   paste0(deparse(call$x), " does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of binarize_df(). Make sure you're supplying the dataframe produced by the binarize_df() function.")
  #
  # }

  assertthat::assert_that(test_15(binarized_soundscape@merged_df))
  assertthat::assert_that(test_16(binarized_soundscape@merged_df))
  # assertthat::assert_that(test_17(binarized_soundscape@merged_df))

  # 1.2.12. The binarized_df argument

  test_18 <- function(x){

    min(x) >= 0 &
      max(x) <= 1

  }

  assertthat::on_failure(test_18) <- function(call, env){

    paste0(deparse(call$x), " has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the binarize_df() function.")

  }

  assertthat::assert_that(test_15(binarized_soundscape@binarized_df))
  assertthat::assert_that(test_16(binarized_soundscape@binarized_df))
  assertthat::assert_that(test_17(binarized_soundscape@binarized_df))
  assertthat::assert_that(test_18(binarized_soundscape@binarized_df))

  # 1.2.13. The aggregated_df data frame and lists

  test_19 <- function(x){

    assertthat::are_equal(x[1, 1], "missing")

  }

  assertthat::on_failure(test_19) <- function(call, env){

    paste0(deparse(call$x), " is not a missing data frame. Did you supply a post-aggregation soundscape to the binarized_soundscape argument of the binarize_df() function? Please supply the output of the binarize_df() function to this argument.")

  }

  assertthat::assert_that(test_19(binarized_soundscape@aggregated_df))

  test_20 <- function(x){

    all(sapply(x, function(x) is.na(x)))

  }

  assertthat::on_failure(test_20) <- function(call, env){

    paste0(deparse(call$x), " is not a list of NAs. Did you supply a post-aggregation soundscape to the binarized_soundscape argument of the binarize_df() function? Please supply the output of the binarize_df() function to this argument.")

  }

  assertthat::assert_that(test_20(binarized_soundscape@aggregated_df_per_time))
  assertthat::assert_that(test_20(binarized_soundscape@effort_per_time))

  # 1.3. Check if the supplied output argument is a string and
  # one of the available options.

  test_21 <- function(x){
    all(assertthat::is.string(x) & (x %in% c("raw", "incidence_freq")))
  }

  assertthat::on_failure(test_21) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available output options. Please consult package documentation for available options.")

  }

  assertthat::assert_that(test_21(output))

  # Get a list of unique times in the dataframe, and sort chronologically

  colnames(binarized_soundscape@binarized_df) <- substr(colnames(binarized_soundscape@binarized_df), 1, 8)

  unique_times <- sort(
    hms::as_hms(
      strptime(
        unique(colnames(binarized_soundscape@binarized_df)),
        format = "%H:%M:%S")))

  # Subset the thresh_df per unique time, add up incidence values per time,
  # and compute the total sampling effort per time. Finally, compute the incidence
  # frequency per OSU for all OSUs in the dataframe

  aggregate_per_time <- vector("list", 0)
  aggregated_df <- vector("list", 0)
  sampling_effort_per_time <- vector("list", 0)

  for (i in 1:length(unique_times)){

    aggregate_per_time[[i]] <- binarized_soundscape@binarized_df[,grepl(as.character(unique_times[i]),
                                                                        x = colnames(binarized_soundscape@binarized_df))] # subset per time

    sampling_effort_per_time[[i]] <- ncol(aggregate_per_time[[i]]) # sampling effort

    aggregated_df[[i]] <- as.data.frame(rowSums(aggregate_per_time[[i]])) # incidence

    if(output=="incidence_freq"){

      aggregated_df[[i]] <- aggregated_df[[i]]/sampling_effort_per_time[[i]]

    }

    else{

      if(output=="raw"){

      }

      else{}

    }

  }


  names(aggregate_per_time) <- unique_times
  names(sampling_effort_per_time) <- unique_times

  aggregated_df <- rlist::list.cbind(aggregated_df)
  colnames(aggregated_df) <- unique_times

  aggregated_soundscape <- new("soundscape",
                               first_day = binarized_soundscape@first_day,
                               lat = binarized_soundscape@lat,
                               lon = binarized_soundscape@lon,
                               tz = binarized_soundscape@tz,
                               sunrise = binarized_soundscape@sunrise,
                               sunset = binarized_soundscape@sunset,
                               fileloc = binarized_soundscape@fileloc,
                               index = binarized_soundscape@index,
                               samplerate = binarized_soundscape@samplerate,
                               window = binarized_soundscape@window,
                               binarization_method = binarized_soundscape@binarization_method,
                               threshold = binarized_soundscape@threshold,
                               output = output,
                               merged_df = binarized_soundscape@merged_df,
                               binarized_df = binarized_soundscape@binarized_df,
                               aggregated_df = aggregated_df,
                               aggregated_df_per_time = aggregate_per_time,
                               effort_per_time = sampling_effort_per_time)

  aggregated_soundscape


}

#' Flexible Soundscape Heatmaps
#'
#' @description  Creates a soundscape heatmap to visualize
#'  the use of acoustic space in the time-frequency domain
#'   for an set acoustic index. The function is highly
#'   flexible, allowing the user maximal control over every
#'    aspect of the heatmap. Please consult the arguments
#'     section to find out more about visualization options.
#'
#' @param aggregated_soundscape The aggregated soundscape object produced by
#'  \code{\link{aggregate_df}} function.
#'
#' @param type One of either "regular" or "polar". If set
#' to "regular", produces a regular rectangular heatmap.
#' If set to "polar", produces a polar heatmap suitable for
#'  exploring diurnal patterns.
#'
#' @param annotate One of either TRUE or FALSE. If set to
#'  TRUE, annotates the heatmap with sunrise and
#'  sunset times,´and highlights the border between the
#'   audible and ultrasonic spectrum for human hearing.
#'
#' @param timeinterval A time interval for the x-axis.
#'  Options can be found in the
#'  \code{\link[scales]{breaks_width}} documentation.
#'
#' @param mintime The lower time limit for the x-axis,
#'  formatted as "HH:MM:SS". Defaults to the earliest
#'  time for which data exists in the dataframe.
#'
#' @param maxtime The upper time limit for the x-axis,
#'  formatted as "HH:MM:SS".Defaults to the latest
#'  time for which data exists in the dataframe.
#'
#' @param freqinterval The frequency interval for the y-axis,
#'  expressed as a numeric value.
#'
#' @param minfreq The lower frequency limit for the y-axis
#' as a numeric value. Defaults to zero.
#'
#' @param maxfreq The upper frequency limit for the y-axis
#' as a numeric value. Defaults to the maximum frequency of
#'  the dataframe.
#'
#' @param nbins If \code{marginplot=TRUE}, determines the
#'  number of the frequency-bins by which to divide the
#'   frequency range to compute the soundscape richness
#'   (q=0), expressed as a single positive integer.
#'
#' @param twilight A character string of the twilight
#' method to be used for sunrise and sunset annotation.
#' Options can be found in the
#'  \code{\link[photobiology]{day_night}} documentation.
#'
#' @param labelsize_time If annotate=TRUE, can be used to
#'  alter the size of temporal spectrum annotation. Please
#'   provide as a single positive integer with a value > 0.
#'
#' @param labelsize_frequency If annotate=TRUE, can be used
#'  to alter the size of frequency spectrum annotation.
#'  Please provide as a single positive integer with a
#'  value > 0.
#'
#' @param labelsize_polar If type="polar", can be used to
#'  alter the size of the frequency axis labels. Please
#'   provide as a single positive integer with a value > 0.
#'
#' @param palette A character string indicating the
#' colormap option to use. Four options are available:
#'  "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"),
#'  "viridis" (or "D", the default option) and "cividis"
#'  (or "E"). Consult \url{https://www.rdocumentation.org/packages/viridisLite/versions/0.3.0/topics/viridis} for options.
#'
#' @param direction Sets the order of colors in the scale.
#' If 1, the default, the regular order is followed.
#' If -1, the order of colors is reversed.
#'
#' @param zero.black One of either TRUE or FALSE.
#' If set to TRUE, absent OSUs with incidence zero
#' will be colored black
#'
#' @param marginplot One of either TRUE or FALSE.
#' If set to TRUE, adds marginal plots to the x- and y-axes.
#' For the x-axis, the marginal plot displays the smoothed
#'  richness of acoustically active frequency bins for
#'  each time of day. For the y-axis, the marginal plot
#'   displays the smoothed richness of acoustically
#'   active time-bins for each frequency band. Note that
#'    marginal plots are not available for type="polar".
#'
#' @param n_time If marginplot=TRUE, determines the backward
#'  window length for smoothing the temporal richness.
#'  Please supply the argument as a single positive integer.
#'
#' @param n_freq If marginplot=TRUE, determines the backward
#'  window length for smoothing the frequency richness.
#'  Please supply the argument as a single positive integer.
#'
#' @param interactive One of either TRUE or FALSE.
#' If set to TRUE, an interactive plot is produced using
#'  \code{\link[plotly]{ggplotly}}. Note that interactive
#'   plots are not available for marginplot=TRUE.
#'
#' @param save One of either TRUE or FALSE. If set to TRUE,
#'  saves the plot using \code{\link[ggplot2]{ggsave}}, and
#'   the 'dir', 'filename' and 'device' arguments.
#'
#' @param dir Path of the directory to save plot to:
#' path and filename are combined to create the
#' fully qualified file name. Defaults to the working
#'  directory. For more information consult
#'   \code{\link[ggplot2]{ggsave}}.
#'
#' @param filename The file name without the extension. For
#'  more information consult \code{\link[ggplot2]{ggsave}}.
#'
#' @param device Device to use. Can either be a device
#'  function (e.g. png()), or one of "eps", "ps",
#'  "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp",
#'   "svg" or "wmf" (windows only). Defaults to "png".
#'   For more information consult
#'    \code{\link[ggplot2]{ggsave}}.
#'
#' @param width If save=TRUE, expresses the width of the
#'  saved image in milimeters. Defaults to 100 mm.
#'
#' @param height If save=TRUE, expresses the height of the
#'  saved image in milimeters. Defaults to 100 mm.
#'
#' @return Returns a ggplot heatmap object and if save=TRUE,
#'  saves the plot in a directory of choice using a
#'  specified device and filename.
#'
#' @export

heatmapper=function(aggregated_soundscape,
                    type="regular",
                    annotate=FALSE,
                    timeinterval="1 hour",
                    mintime="default",
                    maxtime="default",
                    freqinterval=1000,
                    minfreq=0,
                    maxfreq="default",
                    nbins=10,
                    twilight="sunlight",
                    labelsize_time=4,
                    labelsize_frequency=4,
                    labelsize_polar=3,
                    palette="D",
                    direction=1,
                    zero.black=FALSE,
                    marginplot=FALSE,
                    n_time=10,
                    n_freq=2,
                    interactive=FALSE,
                    save=FALSE,
                    dir="default",
                    filename="file",
                    device="png",
                    width=100,
                    height=100){

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(aggregated_soundscape))

  # 1. Check if function input meets expectations

  # 1.1. The supplied aggregated_soundscape argument is an S4-object of the type
  # 'soundscape', and is not empty.

  test_1 <- function(x){

    isS4(x) &
      assertthat::are_equal(class(x)[1], "soundscape") &
      assertthat::not_empty(x)

  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " is not an S4-object of the type 'soundscape', or is empty. Please supply the aggregated_soundscape object produced by the aggregate_df() function. Consult the package documentation for further information.")

  }

  assertthat::assert_that(test_1(aggregated_soundscape))

  # 1.2. The aggregated_soundscape elements are in the expected format

  # 1.2.1. The first_day argument cannot be wrong (S4 property)

  # 1.2.2. The lat and lon argument

  test_3 <- function(x){

    is.numeric(x) &
      x >= -90 &
      x <= 90

  }

  test_4 <- function(x){

    is.numeric(x) &
      x >= -180 &
      x <= 180

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_4) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_3(aggregated_soundscape@lat))
  assertthat::assert_that(test_4(aggregated_soundscape@lon))

  # 1.2.3. The time zone argument

  test_5 <- function(x){

    assertthat::is.string(x) & (x %in% (OlsonNames()))

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")

  }

  assertthat::assert_that(test_5(aggregated_soundscape@tz))

  # 1.2.4. The sunrise and sunset arguments cannot be wrong (s4 property)

  #   # 1.2.5. The fileloc argument
  #
  # test_6 <- function(x){
  #
  #   assertthat::is.dir(x) & assertthat::is.readable(x)
  #
  # }
  #
  # assertthat::assert_that(test_6(aggregated_soundscape@fileloc))

  # 1.2.6. The index argument

  test_7 <- function(x){

    assertthat::is.string(x) & (x %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
                                         "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"))

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available index options. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")

  }

  assertthat::assert_that(test_7(aggregated_soundscape@index))

  # 1.2.7. The samplerate and window arguments

  test_8 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the samplerate and window arguments.")

  }

  assertthat::assert_that(test_8(aggregated_soundscape@samplerate))
  assertthat::assert_that(test_8(aggregated_soundscape@window))

  # 1.2.8. The binarization_method argument

  test_9 <- function(x){
    assertthat::is.string(x) & (x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li","MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu","Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode", "custom"))
  }



  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_9(aggregated_soundscape@binarization_method))

  # 1.2.9. The threshold argument

  test_10 <- function(x){

    all(length(x) == 1 &
          is.double(x) & !is.na(x))

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not a single numeric value. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the value argument is you're supplying a custom threshold value.")

  }

  assertthat::assert_that(test_10(aggregated_soundscape@threshold))

  # 1.2.10. The output argument

  test_11 <- function(x){

    all(length(x) == 1 & is.character(x) & (x %in% c("incidence_freq", "raw")))

  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not a character string describing one of the available output options. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.")

  }

  assertthat::assert_that(test_11(aggregated_soundscape@output))

  # 1.2.11. The merged_df argument

  test_12 <- function(x){

    is.data.frame(x) &
      assertthat::not_empty(x) &
      assertthat::noNA(x) &
      limma::isNumeric(x)

  }

  test_13 <- function(x){

    (abs(as.numeric(rownames(x)[1]))+
       abs(as.numeric(rownames(x)[2])))>3 &
      min(as.numeric(rownames(x))) >= 0 &
      max(as.numeric(rownames(x)))<= aggregated_soundscape@samplerate/2

  }

  test_14 <- function(x){

    formatted <-  try(
      as.POSIXct(
        paste0(substr(aggregated_soundscape@first_day, 1, 12)," ", colnames(x)),
        tz = aggregated_soundscape@tz,
        format="%Y-%m-%d %H:%M:%S"),
      silent = TRUE)

    !any(sapply(formatted, function(y) is.na(y)))

  }


  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.")

  }

  assertthat::on_failure(test_13) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.")

  }

  assertthat::on_failure(test_14) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.")

  }

  assertthat::assert_that(test_12(aggregated_soundscape@merged_df))
  assertthat::assert_that(test_13(aggregated_soundscape@merged_df))
  assertthat::assert_that(test_14(aggregated_soundscape@merged_df))

  # 1.2.12. The binarized_df argument

  test_15 <- function(x){

    min(x) >= 0 &
      max(x) <= 1

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the aggregate_df() function.")

  }

  assertthat::assert_that(test_12(aggregated_soundscape@binarized_df))
  assertthat::assert_that(test_13(aggregated_soundscape@binarized_df))
  assertthat::assert_that(test_14(aggregated_soundscape@binarized_df))
  assertthat::assert_that(test_15(aggregated_soundscape@binarized_df))

  # 1.2.12. The aggregated_df argument

  assertthat::assert_that(test_12(aggregated_soundscape@aggregated_df))
  assertthat::assert_that(test_13(aggregated_soundscape@aggregated_df))
  assertthat::assert_that(test_14(aggregated_soundscape@aggregated_df))

  if(aggregated_soundscape@output=="incidence_freq"){

    test_16 <- function(x){

      all(is.double(unlist(x)) & max(x) <= 1 & min(x)>= 0)

    }

    assertthat::on_failure(test_16) <- function(call, env){

      paste0(deparse(call$x), " contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument.")

    }

    assertthat::assert_that(test_16(aggregated_soundscape@aggregated_df))
  }

  if(aggregated_soundscape@output=="raw"){

    test_16 <- function(x){

      all(all(round(unlist(x)) == unlist(x)) &
            max(x) <= max(table(colnames(aggregated_soundscape@merged_df))) &
            min(x) >= 0)

    }

    assertthat::on_failure(test_16) <- function(call, env){

      paste0(deparse(call$x), " contains values smaller than zero, or larger than the maximum number of soundscape samples per time. The expected range of raw values ranges between 0 and the maximum number of soundscape samples (24-hour recording days). Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument.")

    }

    assertthat::assert_that(test_16(aggregated_soundscape@aggregated_df))
  }

  # 1.2.13. The aggregated_df_per_time argument

  # test_17 <- function(x){
  #
  #   all(
  #
  #   all(sapply(x, function(x) is.data.frame(x))) &
  #     assertthat::are_equal(
  #       as.vector(sort(table(colnames(aggregated_soundscape@merged_df)))),
  #       as.vector(unlist(sapply(x, function(x) ncol(x))))
  #     ) &
  #     all(sapply(x, function(x) nrow(x)==nrow(aggregated_soundscape@merged_df))) &
  #     all(names(x) == unique(colnames(aggregated_soundscape@merged_df))) &
  #     length(x) == ncol(aggregated_soundscape@aggregated_df)
  #
  #   )
  #
  # }
  #
  # assertthat::on_failure(test_17) <- function(call, env){
  #
  #   paste0(deparse(call$x), " does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.")
  #
  # }

  # assertthat::assert_that(test_17(aggregated_soundscape@aggregated_df_per_time))

  # 1.2.14. The effort_per_time argument
  #
  #   test_18 <- function(x){
  #
  #     identical(as.list(sort(table(colnames(aggregated_soundscape@merged_df)))), x)
  #
  #   }
  #
  #   assertthat::on_failure(test_18) <- function(call, env){
  #
  #     paste0(deparse(call$x), " does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.")
  #
  #   }
  #
  #   assertthat::assert_that(test_18(aggregated_soundscape@effort_per_time))

  # 1.3. Check if supplied type argument is one of
  # available options

  test_19 <- function(x){
    assertthat::is.string(x)
  }

  test_20 <- function(x){
    x %in% c("regular", "polar")
  }

  assertthat::on_failure(test_19) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the heatmap type as a character string. Consult package documentation for available type argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_20) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available heatmap type options. Please consult package documentation for available type argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_19(type))
  assertthat::assert_that(test_20(type))

  # 1.4. Check if supplied annotate argument is one of the
  # available options

  test_21 <- function(x){

    assertthat::is.flag(x)

  }

  assertthat::on_failure(test_21) <- function(call, env){

    paste0(deparse(call$x), " is not a Boolean flag (TRUE or FALSE). Please set annotate argument to TRUE or FALSE. Make sure the argument is not a character string.")

  }

  assertthat::assert_that(test_21(annotate))

  # 1.5. Check if supplied timeinterval argument is one of
  # the available options

  test_22 <- function(x){

    any(stringr::str_detect(timeinterval,c("sec", "secs", "min", "mins", "hour", "hours", "day", "days", "week", "weeks", "month", "months", "year", "years"))) &
      grepl("^[[:digit:]]\\s", timeinterval)

  }

  assertthat::on_failure(test_22) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available timeinterval options. Please make sure the timeinterval argument is a character string of the following format: n unit (with n = number, and unit = one of 'sec', 'secs', 'min', 'mins', 'hour', 'hours', 'day', 'days', 'week', 'weeks', 'month', 'months', 'year', 'years'). Please consult the scales::breaks_width() documentation for more information.")

  }

  assertthat::assert_that(test_22(timeinterval))

  # 1.6. Check if supplied mintime and maxtime arguments
  # are one of the available options

  test_23 <- function(x){

    x == "default" |
      !is.na(as.POSIXct(x, format="%H:%M:%S"))

  }

  assertthat::on_failure(test_23) <- function(call, env){

    paste0(deparse(call$x), " is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.")

  }

  assertthat::assert_that(test_23(mintime))
  assertthat::assert_that(test_23(maxtime))

  # 1.7. Check if the freqinterval argument is in the right format.

  test_24 <- function(x){

    assertthat::is.count(x) &
      x > min(as.numeric(rownames(aggregated_soundscape@aggregated_df))) &
      x < max(as.numeric(rownames(aggregated_soundscape@aggregated_df)))

  }

  assertthat::on_failure(test_24) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer, or is outside of the frequency bounds of the data frame. Please supply the frequency interval as a single positive integer which falls without the data frame's frequency bounds (min frequency < freqinterval < max frequency).")

  }

  assertthat::assert_that(test_24(freqinterval))

  # 1.8. Check if the minfreq and maxfreq arguments follow
  # the expected values

  test_25 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregated_soundscape@aggregated_df))) &
       x <= max(as.numeric(rownames(aggregated_soundscape@aggregated_df)))) |
      x == 0

  }

  test_26 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregated_soundscape@aggregated_df))) &
       x <= max(as.numeric(rownames(aggregated_soundscape@aggregated_df)))) |
      x == "default"

  }

  assertthat::on_failure(test_25) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.")

  }

  assertthat::on_failure(test_26) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.")

  }

  assertthat::assert_that(test_25(minfreq))
  assertthat::assert_that(test_26(maxfreq))

  # 1.9. Check if the nbins argument abides by the
  # expected format

  test_27 <- function(x){

    assertthat::is.count(x) &
      x > 0 &
      x < nrow(aggregated_soundscape@aggregated_df)
  }

  assertthat::on_failure(test_27) <- function(call, env){

    paste0(deparse(call$x), " is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the marginplot argument. If you wish to display a marginal plot, please set marginplot = TRUE.")

  }

  assertthat::assert_that(test_27(nbins))


  # 1.10. Check if the supplied twilight argument is one
  # of the available options

  test_28 <- function(x){

    (assertthat::is.string(x) &
       x %in% c("none","rim","refraction","sunlight","civil",
                "nautical","astronomical")) |
      (is.vector(x, mode="double") & (length(x) == 1 | length(x) ==2))

  }

  assertthat::on_failure(test_28) <- function(call, env){

    paste0(deparse(call$x), " is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.")

  }

  assertthat::assert_that(test_28(twilight))

  # 1.11. Check if the labelsize arguments follow the
  # expected format.

  test_29 <- function(x){

    (abs(x) == x) &
      x > 0 &
      length(x) == 1

  }

  assertthat::on_failure(test_29) <- function(call, env){

    paste0(deparse(call$x), " is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.")

  }

  assertthat::assert_that(test_29(labelsize_time))
  assertthat::assert_that(test_29(labelsize_frequency))
  assertthat::assert_that(test_29(labelsize_polar))

  # 1.12. Check if the palette and direction arguments
  # abide by the expected format.

  test_30 <- function(x){

    assertthat::is.string(x) &
      x %in% c("A", "B", "C", "D", "E", "magma", "inferno",
               "plasma", "viridis", "cividis")

  }

  test_31 <- function(x){

    x %in% c(1, -1)

  }

  assertthat::on_failure(test_30) <- function(call, env){

    paste0(deparse(call$x), " is not a valid palette argument. Palette needs to be supplied as a character string of either: 'A', 'B', 'C', 'D', 'E', 'magma', 'inferno', 'plasma', 'viridis' or 'cividis'. Please supply a valid palette argument. Consult the soundscapeR of viridis package documentation for more information.")

  }

  assertthat::on_failure(test_31) <- function(call, env){

    paste0(deparse(call$x), " is not a valid direction argument. The direction argument needs to be supplied as a single integer of either 1 or -1. Please supply a valid direction argument. Consult the soundscapeR of viridis package documentation for more information.")

  }

  assertthat::assert_that(test_30(palette))
  assertthat::assert_that(test_31(direction))

  # 1.13. Check if the boolean flag arguments follow the
  # expected format (zero.black, marginplot, interactive,
  # save)

  test_32 <- function(x){

    assertthat::is.flag(x)

  }

  assertthat::on_failure(test_32) <- function(call, env){

    paste0(deparse(call$x), " is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.")

  }

  assertthat::assert_that(test_32(zero.black))
  assertthat::assert_that(test_32(marginplot))
  assertthat::assert_that(test_32(interactive))
  assertthat::assert_that(test_32(save))

  test_33 <- function(x){

    if (x == TRUE){

      type =="regular" &
        interactive ==FALSE

    } else{return(TRUE)}

  }

  assertthat::on_failure(test_33) <- function(call, env){

    paste0(deparse(call$x), " is used with other arguments which are not accepted. The marginplot=TRUE argument can not be used in synergy with type='polar' or interactive = TRUE.")

  }

  assertthat::assert_that(test_33(x = marginplot))

  # 1.14. Check if the n_time and the n_freq arguments
  # follow the expected format

  test_34 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_34) <- function(call, env){
    paste0(deparse(call$x), " does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.")

  }

  assertthat::assert_that(test_34(n_time))
  assertthat::assert_that(test_34(n_freq))

  # 1.15. Check if the dir, filename and device arguments
  # follow the expected format.

  if (dir=="default"){
    dir <- getwd()
  }

  else{dir <- dir}

  test_35 <- function(x){

    assertthat::is.string(x)

  }

  test_36 <- function(x){

    assertthat::is.dir(x)

  }

  assertthat::on_failure(test_35) <- function(call, env){
    paste0(deparse(call$x), " is not a character string. The dir arguments needs to be a character string of either 'default' - or a valid pathname to an existing directory on your device. If you're working on a Windows operating system, pay attention to backslash and forwardslash.")

  }

  assertthat::assert_that(test_35(dir))
  assertthat::assert_that(test_36(dir))

  test_37 <- function(x){

    assertthat::is.string(x)

  }

  test_38 <- function(x){

    !(sub('.*\\.', '', filename) %in% c("eps", "ps","tex", "pdf", "jpeg",
                                        "tiff","png", "bmp","svg", "wmf"))

  }

  assertthat::on_failure(test_37) <- function(call, env){
    paste0(deparse(call$x), " is not a valid filename argument. The filename argument needs to be a character string.")
  }

  assertthat::on_failure(test_38) <- function(call, env){
    paste0(deparse(call$x), " is not a valid filename argument. Please make the filename argument you provide a character string without the extension.")
  }

  assertthat::assert_that(test_37(filename))
  assertthat::assert_that(test_38(filename))

  test_39 <- function(x){

    assertthat::is.string(x) &
      x %in% c("eps", "ps","tex", "pdf", "jpeg", "tiff",
               "png", "bmp","svg", "wmf")

  }

  assertthat::on_failure(test_39) <- function(call, env){
    paste0(deparse(call$x), " is not a valid device argument. The device argument needs to be a character string, and one of the following options: eps, ps, tex, pdf, jpeg, tiff, png, bmp, svg, wmf.")
  }

  assertthat::assert_that(test_39(device))

  # 1.16. Check if the supplied height and width arguments
  # follow the expected format

  test_40 <- function(x){
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_40) <- function(call, env){
    paste0(deparse(call$x), " is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.")
  }

  assertthat::assert_that(test_40(height))
  assertthat::assert_that(test_40(width))

  # 2. Prepare variables for plotting

  tz <- aggregated_soundscape@tz

  lengthen=function(aggregated_soundscape){

    tz <- aggregated_soundscape@tz
    df <- aggregated_soundscape@aggregated_df
    df$frequency <- as.integer(rownames(df))
    melt_df <- reshape2::melt(df, id.vars="frequency")
    colnames(melt_df) <- c("frequency", "time", "value")
    melt_df$frequency <- as.numeric(
      as.character(melt_df$frequency))
    melt_df$time <- as.POSIXct(
      strptime(
        x =paste(substr(aggregated_soundscape@first_day, 1, 12),
                 melt_df$time,
                 sep=" "),
        format = "%Y-%m-%d %H:%M",
        tz = tz))
    return(melt_df)

  }

  df2 <- lengthen(aggregated_soundscape = aggregated_soundscape)

  if(zero.black==TRUE){
    df2[df2==0] <- NA
  }

  else{}

  if (mintime=="default"){
    mintime <- min(
      as.POSIXct(
        strptime(
          paste(substr(aggregated_soundscape@first_day, 1, 12),
                colnames(aggregated_soundscape@aggregated_df),
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz = tz)))
  }

  else{
    mintime <- as.POSIXct(
      strptime(
        paste(substr(aggregated_soundscape@first_day, 1, 12),
              mintime,
              sep=" "),
        format= "%Y-%m-%d %H:%M:%S",
        tz = tz))

  }

  if (maxtime=="default"){
    maxtime <- max(
      as.POSIXct(
        strptime(
          paste(substr(aggregated_soundscape@first_day, 1, 12),
                colnames(aggregated_soundscape@aggregated_df),
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz = tz)))

  }

  else{
    maxtime <- as.POSIXct(
      strptime(
        paste(substr(aggregated_soundscape@first_day, 1, 12),
              maxtime,
              sep=" "),
        format= "%Y-%m-%d %H:%M:%S",
        tz = tz))
  }

  if (maxfreq=="default"){
    maxfreq <- max(df2$frequency)
  }

  else{maxfreq <- maxfreq}

  day <- as.POSIXct(
    strptime(
      paste(substr(aggregated_soundscape@first_day, 1, 12),
            "00:00:00",
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz = tz))

  sunrise <- aggregated_soundscape@sunrise
  sunset <- aggregated_soundscape@sunset

  midnight1 <- as.POSIXct(
    strptime(
      paste(substr(aggregated_soundscape@first_day, 1, 12),
            "00:00:00",
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz = tz))

  midnight2 <- as.POSIXct(
    strptime(
      paste(substr(aggregated_soundscape@first_day, 1, 12),
            "23:55:00",
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz = tz))

  if (type=="regular"){

    if (annotate==TRUE){

      if (maxfreq>22000){

        plot <-
          ggplot2::ggplot(df2,
                          ggplot2::aes(time,
                                       frequency,
                                       fill=value,
                                       color=value)) +

          ggplot2::geom_tile()+
          viridis::scale_fill_viridis(
            option=palette,
            na.value="black",
            direction = direction,
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=1),
            breaks=seq(0, 1, 0.1),
            limits = c(0, 1))+

          viridis::scale_color_viridis(
            option=palette,
            na.value="black",
            direction = direction,
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=2),
            breaks=seq(0, 1, 0.1),
            limits = c(0, 1)
          ) +


          ggplot2::scale_x_datetime(
            labels=scales::date_format("%H:%M", tz=tz),
            breaks = scales::breaks_width(timeinterval),
            expand = c(0,0),
            limits = c(mintime,maxtime))+

          ggplot2::scale_y_continuous(
            limits = c(minfreq, (maxfreq+(maxfreq/10))),
            expand = c(0,0),
            breaks = seq(minfreq, maxfreq, freqinterval),
            label=comma)+

          ggplot2::labs(
            y="Frequency (Hz)",
            x="Time (hour of day)")+

          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(
              colour = "black"),
            axis.text.y = ggplot2::element_text(
              color = "black",
              size = 10),
            axis.text.x = ggplot2::element_text(
              color = "black",
              size = 10,
              angle = 45,
              hjust=1.1,
              vjust=1),
            axis.title.y = ggplot2::element_text(margin = unit(c(0, 3, 0, 0), "mm")),
            axis.title.x = ggplot2::element_text(margin = unit(c(3, 0, 0, 0), "mm")),
            plot.margin = grid::unit(c(1,1,1,1),"cm"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.title = ggplot2::element_text(
              color = "black",
              size = 12,
              face = "bold"))+

          ggplot2::geom_vline(
            ggplot2::aes(xintercept = as.numeric(aggregated_soundscape@sunrise)),
            linetype="dashed",
            color= if (direction==1){paste("white")}
            else{paste("black")})+

          ggplot2::geom_vline(
            ggplot2::aes(xintercept = as.numeric(aggregated_soundscape@sunset)),
            linetype="dashed",
            color= if (direction==1){paste("white")}
            else{paste("black")})+

          ggplot2::geom_hline(
            yintercept = 20000,
            linetype="dashed",
            color= if (direction==1){paste("white")}
            else{paste("black")})+

          ggplot2::annotate(
            geom="rect",
            xmin =min(df2$time),
            xmax=sunrise,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69",
            color="white",
            alpha=1)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunset,
            xmax=midnight2,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69",
            color="white",
            alpha=1)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunrise,
            xmax=sunset,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#ffcc13",
            color="white",
            alpha=1)+

          ggplot2::annotate(
            geom="rect",
            xmin =min(df2$time),
            xmax=sunrise,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69",
            color="black",
            alpha=0.25)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunset,
            xmax=(midnight2-(60*5)),
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69",
            color="black",
            alpha=0.25)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunrise,
            xmax=sunset,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#ffcc13",
            color="black",
            alpha=0.25)+

          ggplot2::annotate(
            geom="rect",
            xmin =midnight2,
            xmax=midnight2+3600,
            ymin=minfreq,
            ymax=20000,
            fill="white",
            color="white",
            alpha=0.5)+

          ggplot2::annotate(
            geom="rect",
            xmin =midnight2,
            xmax=midnight2+3600,
            ymin=20000,
            ymax=maxfreq,
            fill="white",
            color="white",
            alpha=0.5)+

          ggplot2::annotate(
            "text",
            x = (sunrise-(as.numeric(
              difftime(sunrise,
                       midnight1,
                       units = "secs"))/2)),
            y = (maxfreq+(maxfreq/20)),
            label = "NIGHTTIME",
            color="white",
            fontface=2,
            size=labelsize_time)+

          ggplot2::annotate(
            "text",
            x = (sunset-(as.numeric(
              difftime(sunset,
                       sunrise,
                       units = "secs"))/2)),
            y = (maxfreq+(maxfreq/20)),
            label = "DAYTIME",
            color="black",
            fontface=2,
            size=labelsize_time)+

          ggplot2::annotate(
            "text",
            x = (midnight2-(as.numeric(
              difftime(midnight2,
                       sunset,
                       units = "secs"))/2)),
            y = (maxfreq+(maxfreq/20)),
            label = "NIGHTTIME",
            color="white",
            fontface=2,
            size=labelsize_time)+

          ggplot2::geom_vline(xintercept = midnight2) +

          ggplot2::annotate(
            "text",
            x = midnight2+2200,
            y = 20000+((maxfreq-20000)/2),
            label = "ULTRASOUND",
            color="black",
            fontface=2,
            angle=-90,
            size=labelsize_frequency)+

          ggplot2::annotate(
            "text",
            x = midnight2+2200,
            y = (20000-minfreq)/2,
            label = "AUDIBLE",
            color="black",
            fontface=2,
            angle=-90,
            size=labelsize_frequency)+

          ggplot2::labs(fill="OSU INCIDENCE") +

          guides(color='none')
      }

      else{

        if (maxfreq<=22000){

          plot <-

            ggplot2::ggplot(
              df2,
              ggplot2::aes(time,
                           frequency,
                           fill=value,
                           color=value)) +

            ggplot2::geom_tile()+

            viridis::scale_fill_viridis(
              option=palette,
              na.value="black",
              direction = direction,
              guide = ggplot2::guide_legend(
                title.position = "top",
                title.vjust = 1,
                title.hjust = 0.5,
                nrow=1),
              breaks=seq(0, 1, 0.1),
              limits = c(0, 1))+

            viridis::scale_color_viridis(
              option=palette,
              na.value="black",
              direction = direction,
              guide = ggplot2::guide_legend(
                title.position = "top",
                title.vjust = 1,
                title.hjust = 0.5,
                nrow=2),
              breaks=seq(0, 1, 0.1), limits = c(0, 1)
            ) +

            ggplot2::scale_x_datetime(
              labels=scales::date_format("%H:%M", tz=tz),
              breaks = scales::breaks_width(timeinterval),
              expand = c(0,0),
              limits = c(mintime,maxtime))+

            ggplot2::scale_y_continuous(
              limits = c(minfreq, (maxfreq+(maxfreq/10))),
              expand = c(0,0),
              breaks = seq(minfreq, maxfreq, freqinterval),
              label=comma)+

            ggplot2::labs(
              y="Frequency (Hz)",
              x="Time (hour of day)")+

            ggplot2::theme(
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              axis.line = ggplot2::element_line(
                colour = "black"),
              axis.text.y = ggplot2::element_text(
                color = "black", size = 10),
              axis.text.x = ggplot2::element_text(
                color = "black",
                size = 10,
                angle = 45,
                hjust=1.1,
                vjust=1),
              axis.title.y = ggplot2::element_text(margin = unit(c(0, 3, 0, 0), "mm")),
              axis.title.x = ggplot2::element_text(margin = unit(c(3, 0, 0, 0), "mm")),
              plot.margin = grid::unit(c(1,1,1,1),"cm"),
              legend.position = "top",
              legend.direction = "horizontal",
              legend.title = ggplot2::element_text(
                color = "black",
                size = 12,
                face = "bold"))+

            ggplot2::geom_vline(
              ggplot2::aes(xintercept = as.numeric(aggregated_soundscape@sunrise)),
              linetype="dashed",
              color= if (direction==1){paste("white")}
              else{paste("black")})+

            ggplot2::geom_vline(
              ggplot2::aes(xintercept = as.numeric(aggregated_soundscape@sunset)),
              linetype="dashed",
              color= if (direction==1){paste("white")}
              else{paste("black")})+

            ggplot2::geom_hline(
              yintercept = 20000,
              linetype="dashed",
              color=if (direction==1){paste("white")}
              else{paste("black")})+

            ggplot2::annotate(
              geom="rect",
              xmin =min(df2$time),
              xmax=sunrise,
              ymin=maxfreq,
              ymax=(maxfreq+(maxfreq/10)),
              fill="#4C4B69",
              color="white",
              alpha=1)+

            ggplot2::annotate(
              geom="rect",
              xmin =sunset,
              xmax=midnight2,
              ymin=maxfreq,
              ymax=(maxfreq+(maxfreq/10)),
              fill="#4C4B69",
              color="white",
              alpha=1)+

            ggplot2::annotate(
              geom="rect",
              xmin =sunrise,
              xmax=sunset,
              ymin=maxfreq,
              ymax=(maxfreq+(maxfreq/10)),
              fill="#ffcc13",
              color="white",
              alpha=1)+

            ggplot2::annotate(
              geom="rect",
              xmin =min(df2$time),
              xmax=sunrise,
              ymin=maxfreq,
              ymax=(maxfreq+(maxfreq/10)),
              fill="#4C4B69",
              color="black",
              alpha=0.25)+

            ggplot2::annotate(
              geom="rect",
              xmin =sunset,
              xmax=midnight2,
              ymin=maxfreq,
              ymax=(maxfreq+(maxfreq/10)),
              fill="#4C4B69",
              color="black",
              alpha=0.25)+

            ggplot2::annotate(
              geom="rect",
              xmin =sunrise,
              xmax=sunset,
              ymin=maxfreq,
              ymax=(maxfreq+(maxfreq/10)),
              fill="#ffcc13",
              color="black",
              alpha=0.25)+

            ggplot2::annotate(
              geom="rect",
              xmin =midnight2,
              xmax=midnight2+3600,
              ymin=minfreq,
              ymax=20000,
              fill="white",
              color="white",
              alpha=0.5)+

            ggplot2::annotate(
              geom="rect",
              xmin =midnight2,
              xmax=midnight2+3600,
              ymin=20000,
              ymax=maxfreq,
              fill="white",
              color="white",
              alpha=0.5)+

            ggplot2::annotate(
              "text",
              x = (sunrise-(as.numeric(
                difftime(sunrise,
                         midnight1,
                         units = "secs"))/2)),
              y = (maxfreq+(maxfreq/20)),
              label = "NIGHTTIME",
              color="white",
              fontface=2)+

            ggplot2::annotate(
              "text", x = (sunset-(as.numeric(
                difftime(sunset,
                         sunrise,
                         units = "secs"))/2)),
              y = (maxfreq+(maxfreq/20)),
              label = "DAYTIME",
              color="black",
              fontface=2)+

            ggplot2::annotate(
              "text",
              x = (midnight2-(as.numeric(
                difftime(midnight2,
                         sunset,
                         units = "secs"))/2)),
              y = (maxfreq+(maxfreq/20)),
              label = "NIGHTTIME",
              color="white",
              fontface=2)+

            ggplot2::geom_vline(xintercept = midnight2)+

            ggplot2::labs(fill="OSU INCIDENCE")+

            guides(color='none')
        }
      }
    }

    else {

      if (annotate==FALSE){

        plot <-
          ggplot2::ggplot(
            df2, ggplot2::aes(time,
                              frequency,
                              fill=value,
                              color=value)) +

          ggplot2::geom_tile()+

          viridis::scale_fill_viridis(
            option=palette,
            na.value="black",
            direction = direction,
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=1),
            breaks=seq(0, 1, 0.1), limits = c(0, 1))+

          viridis::scale_color_viridis(
            option=palette,
            na.value="black",
            direction = direction,
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=2),
            breaks=seq(0, 1, 0.1), limits = c(0, 1)
          ) +

          ggplot2::scale_x_datetime(
            labels=scales::date_format("%H:%M", tz=tz),
            breaks = scales::breaks_width(timeinterval),
            expand = c(0,0),
            limits = c(mintime,maxtime))+

          ggplot2::scale_y_continuous(
            limits = c(minfreq,maxfreq),
            expand = c(0,0),
            breaks = seq(minfreq, maxfreq, freqinterval),
            label=comma)+

          ggplot2::labs(
            y="Frequency (Hz)",
            x="Time (hour of day)")+

          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(
              colour = "black"),
            axis.text.y = ggplot2::element_text(
              color = "black", size = 10),
            axis.text.x = ggplot2::element_text(
              color = "black",
              size = 10,
              angle = 45,
              hjust=1.1,
              vjust=1),
            axis.title.y = ggplot2::element_text(margin = unit(c(0, 3, 0, 0), "mm")),
            axis.title.x = ggplot2::element_text(margin = unit(c(3, 0, 0, 0), "mm")),
            panel.border = ggplot2::element_rect(
              colour = "black",
              fill=NA,
              size=0.5),
            plot.margin = grid::unit(c(1,1,1,1),"cm"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.title = ggplot2::element_text(
              color = "black",
              size = 12,
              face = "bold"))+

          ggplot2::labs(fill="OSU INCIDENCE")+

          guides(color='none')

      }
    }
  }

  else{

    if (type=="polar"){

      if (annotate==TRUE){

        plot <-
          ggplot2::ggplot(
            df2, ggplot2::aes(time,
                              frequency,
                              fill=value,
                              color=value)) +

          ggplot2::geom_tile()+

          viridis::scale_fill_viridis(
            option=palette,
            na.value="black",
            direction = direction,
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=2),
            breaks=seq(0, 1, 0.1), limits = c(0, 1))+

          viridis::scale_color_viridis(
            option=palette,
            na.value="black",
            direction = direction,
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=2),
            breaks=seq(0, 1, 0.1), limits = c(0, 1)
          ) +

          ggplot2::scale_x_datetime(
            labels=scales::date_format("%H:%M", tz=tz),
            breaks = scales::breaks_width(timeinterval),
            expand = c(0,0),
            limits = c(mintime,maxtime))+

          ggplot2::scale_y_continuous(
            limits = c(minfreq,(maxfreq+(maxfreq/10))),
            expand = c(0,0),
            breaks = seq(minfreq, maxfreq, freqinterval),
            label=comma)+

          ggplot2::labs(
            y="Frequency (Hz)",
            x="Time (hour of day)")+

          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(
              colour = "black"),
            axis.text.x = ggplot2::element_text(
              color = "black",
              size = 10,
              angle = -0,
              hjust=1.1,
              vjust=1),
            axis.title.y = ggplot2::element_text(margin = unit(c(0, 3, 0, 0), "mm")),
            axis.title.x = ggplot2::element_text(margin = unit(c(3, 0, 0, 0), "mm")),
            panel.border = ggplot2::element_rect(
              colour = "white",
              fill=NA,
              size=0.5),
            plot.margin = grid::unit(c(1,1,1,1),"cm"),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.title = ggplot2::element_text(
              color = "black",
              size = 12,
              face = "bold"))+

          ggplot2::annotate(
            geom="segment",
            x=sunrise,
            xend=sunrise,
            y=minfreq,
            yend=maxfreq,
            color= if (direction==1){paste("white")}
            else{paste("black")})+

          ggplot2::annotate(
            geom="segment",
            x=sunset,
            xend=sunset,
            y=minfreq,
            yend=maxfreq,
            color= if (direction==1){paste("white")}
            else{paste("black")})+

          ggplot2::annotate(
            geom="segment",
            x=seq.POSIXt(from=min(df2$time),
                         to=max(df2$time),
                         by=3600),
            xend=seq.POSIXt(from=min(df2$time),
                            to=max(df2$time),
                            by=3600),
            y=minfreq,
            yend=maxfreq,
            color="#383e42",
            alpha=0.5,
            size=0.2)+

          ggplot2::coord_polar()+

          ggplot2::geom_hline(
            yintercept = seq(minfreq,
                             maxfreq,
                             freqinterval),
            color="#383e42",
            alpha=0.5,
            size=0.2)+

          ggplot2::annotate(
            geom="label",
            size=labelsize_polar,
            y=seq(minfreq,
                  maxfreq,
                  freqinterval),
            x=min(df2$time),
            label=as.character(seq(minfreq,
                                   maxfreq,
                                   freqinterval)))+

          ggplot2::annotate(
            geom="rect",
            xmin =min(df2$time),
            xmax=sunrise,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69", alpha=1)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunset,
            xmax=midnight2,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69",
            alpha=1)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunrise,
            xmax=sunset,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#ffcc13",
            alpha=1)+

          ggplot2::annotate(
            geom="rect",
            xmin =min(df2$time),
            xmax=sunrise,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69",
            alpha=0.25)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunset,
            xmax=midnight2,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69", alpha=0.25)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunrise,
            xmax=sunset,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#ffcc13",
            alpha=0.25)+

          ggplot2::labs(fill="OSU INCIDENCE")+

          guides(color='none')

      }

      else{

        if (annotate==FALSE){

          plot <-
            ggplot2::ggplot(
              df2,
              ggplot2::aes(time,
                           frequency,
                           fill=value,
                           color=value)) +

            ggplot2::geom_tile()+

            viridis::scale_fill_viridis(
              option=palette,
              na.value="black",
              direction = direction,
              guide = ggplot2::guide_legend(
                title.position = "top",
                title.vjust = 1,
                title.hjust = 0.5,
                nrow=2),
              breaks=seq(0, 1, 0.1), limits = c(0, 1))+

            viridis::scale_color_viridis(
              option=palette,
              na.value="black",
              direction = direction,
              guide = ggplot2::guide_legend(
                title.position = "top",
                title.vjust = 1,
                title.hjust = 0.5,
                nrow=2),
              breaks=seq(0, 1, 0.1), limits = c(0, 1)
            ) +

            ggplot2::scale_x_datetime(
              labels=scales::date_format("%H:%M", tz=tz),
              breaks = scales::breaks_width(timeinterval),
              expand = c(0,0),
              limits = c(mintime,maxtime))+

            ggplot2::scale_y_continuous(
              limits = c(minfreq,maxfreq),
              expand = c(0,0),
              breaks = seq(minfreq,
                           maxfreq,
                           freqinterval),
              label=scales::unit_format(unit = "K"))+

            ggplot2::labs(
              y="Frequency (Hz)",
              x="Time (hour of day)")+

            ggplot2::theme(
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              axis.line = ggplot2::element_line(
                colour = "black"),
              axis.text.x = ggplot2::element_text(
                color = "black",
                size = 10,
                angle = -0,
                hjust=1.1,
                vjust=1),
              axis.title.y = ggplot2::element_text(margin = unit(c(0, 3, 0, 0), "mm")),
              axis.title.x = ggplot2::element_text(margin = unit(c(3, 0, 0, 0), "mm")),
              panel.border = ggplot2::element_rect(
                colour = "white",
                fill=NA,
                size=0.5),
              axis.text.y = ggplot2::element_blank(),
              axis.ticks.y = ggplot2::element_blank(),
              plot.margin = grid::unit(c(1,1,1,1),"cm"),
              legend.position = "top",
              legend.direction = "horizontal",
              legend.title = ggplot2::element_text(
                color = "black",
                size = 12,
                face = "bold"))+

            ggplot2::coord_polar()+

            ggplot2::geom_hline(
              yintercept = seq(minfreq,
                               maxfreq,
                               freqinterval),
              color="#383e42",
              alpha=0.5,
              size=0.2)+

            ggplot2::annotate(
              geom="label",
              size=labelsize_polar,
              y=seq(minfreq,
                    maxfreq,
                    freqinterval),
              x=min(df2$time),
              label=as.character(seq(minfreq,
                                     maxfreq,
                                     freqinterval)))+

            ggplot2::labs(fill="OSU INCIDENCE")+

            guides(color='none')

        }
      }
    }
  }

  if (marginplot==FALSE & interactive==FALSE & save==FALSE){
    plot
  }

  else{

    if (marginplot==FALSE & interactive==FALSE & save==TRUE){

      ggplot2::ggsave(
        filename=paste0(
          paste0(type, "_"),
          "no_margin_",
          filename,
          ".",
          device),
        plot=plot,
        device=device,
        path=dir,
        dpi = "retina",
        width = width,
        height = height,
        units = c("mm"))

      plot
    }

    else{

      if (marginplot==FALSE & interactive==TRUE & save==FALSE){
        plotly::ggplotly(plot)
      }

      else{

        if (marginplot==FALSE & interactive==TRUE & save==TRUE){

          ggplot2::ggsave(
            filename=paste0(
              paste0(
                type,
                "_"),
              "no_margin_",
              filename,
              ".",
              device),
            plot=plot,
            device=device,
            path=dir,
            dpi = "retina",
            width=width,
            height = height,
            units = c("mm"))

          plotly::ggplotly(plot)
        }

        else{

          if (marginplot==TRUE & type=="regular" & interactive==FALSE){

            heatmap <-
              plot+ggplot2::theme(
                plot.margin = grid::unit(
                  c(0.15, 0.15, 0, 0), "cm"),
                legend.position = "none")

            xdata <- sounddiv(aggregated_soundscape = aggregated_soundscape,
                              qvalue = 0,
                              subset = "tod",
                              minfreq = minfreq,
                              maxfreq = maxfreq)

            colnames(xdata) <- c("richness", "time")

            xdata$richness_smooth=pracma::movavg(
              xdata$richness,
              n=n_time,
              type="t")

            xplot <-
              ggplot2::ggplot(
                xdata,
                ggplot2::aes(time,
                             richness_smooth))+

              ggplot2::geom_area(alpha=0.25,
                                 fill="#440154FF")+

              ggplot2::geom_line(color="black",
                                 size=1.2)+

              ggplot2::ylab("Richness (%)")+

              ggplot2::xlab(NULL)+

              ggplot2::scale_x_time(expand = c(0,0))+

              ggplot2::scale_y_continuous(
                expand = c(0,0),
                label=comma)+

              ggplot2::theme(
                axis.title.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_blank(),
                panel.border = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(),
                plot.margin = grid::unit(c(0, 0, 0, 0),
                                         "cm"))

            ydata <-
              as.data.frame(
                sounddiv(
                  aggregated_soundscape = aggregated_soundscape,
                  qvalue = 0,
                  subset = "total",
                  freqseq = TRUE,
                  nbins = nbins,
                  minfreq = minfreq,
                  maxfreq = maxfreq)[,1])

            ydata$frequency <- seq(from = minfreq,
                                   to = maxfreq,
                                   by = maxfreq/(nrow(ydata)-1))

            colnames(ydata) <- c("richness",
                                 "frequency")

            ydata$richness_smooth <- pracma::movavg(
              ydata$richness,
              n=n_freq,
              type="s")

            yplot <-
              ggplot2::ggplot(
                ydata,
                ggplot2::aes(frequency,
                             richness_smooth))+

              ggplot2::geom_area(alpha=0.40,
                                 fill="#FDE725FF")+

              ggplot2::geom_line(color="black",
                                 size=1.5)+

              ggplot2::ylab("Richness (%)")+

              ggplot2::xlab(NULL)+

              ggplot2::scale_x_continuous(
                expand = c(0,0),
                limits = c(0,(maxfreq+(maxfreq/10))))+

              ggplot2::scale_y_continuous(
                expand = c(0,0),
                label=comma)+

              ggplot2::theme(
                axis.title.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank(),
                axis.text.y = ggplot2::element_blank(),
                panel.border = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(),
                plot.margin = grid::unit(c(0, 0, 0, -0),
                                         "cm"))+

              ggplot2::coord_flip()

            combined_plot <-
              xplot +
              patchwork::plot_spacer() +
              heatmap +
              yplot +
              patchwork::plot_layout(widths = c(3, 1),
                                     heights = c(1,3))

            if (save==TRUE){
              ggplot2::ggsave(
                filename=paste0(
                  paste0(type,
                         "_"),
                  "marginplot_",
                  filename,
                  ".",
                  device),
                plot=combined_plot,
                device=device,
                path=dir,
                dpi = "retina",
                width=width,
                height = height,
                units = c("mm"))

              combined_plot
            }

            else{

              if (save==FALSE){
                combined_plot
              }
            }
          }
        }
      }
    }
  }
}


# Functions for soundscape diversity calculations

#' Estimate soundscape diversity using Hill numbers
#'
#' @description For a set acoustic index, calculates the diversity of
#'  acoustically active Operational Sound Units (OSUs) in the soundscape.
#'  The q-parameter can be altered to modulate the diversity metric's
#'   sensitivity to abundance. The soundscape diversity metrics can be
#'    computed at various scales and resolutions. For instance, the user
#'     can explore the diversity for the whole soundscape, specify custom
#'      time and frequency limits, or use one of the built-in presets for
#'       diurnal-phase subsetting (day, night, dawn, dusk). Additionally,
#'       the user can track the change in soundscape diversity throughout
#'       the day. Finally, the soundscape diversity can be assessed for
#'       the entire frequency range, or per frequency-bin of
#'        user-defined width.
#'
#' \strong{Note:} Soundscape diversity metrics should not be used to make
#'  inference about the diversity of the real-world biological community
#'   unless verified using ground-truthing methods.
#'
#' @param aggregated_soundscape The aggregated soundscape object produced by
#'  \code{\link{aggregate_df}} function.
#'
#' @param qvalue A positive integer or decimal number (>=0), most commonly
#'  between 0-3. This parameter modulates the sensitivity of diversity
#'  values to the relative abundance of Operational Sound Units (OSUs).
#'  A value of 0 corresponds to the richness, a value of 1 is the equivalent
#'   effective number of OSUs for the Shannon index, a value of 2 is the
#'    equivalent effective number of OSUs for the Simpson index.
#' @param subset The scale for which the soundscape diversity is computed.
#'  Options are 'total', 'day', night', 'dawn', 'dusk' and
#'  'tod' (time of day - for each unique time in the day).
#' @param minfreq A numeric value indicating the lower frequency limit
#' for which to compute the soundscape diversity. If set to default, uses
#' the lowest available frequency in the dataframe.
#' @param maxfreq A numeric value indicating the upper frequency limit
#' for which to compute the soundscape diversity. If set to default,
#' uses the highest available frequency in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape
#'  diversity, formatted as "HH:MM:SS". If set to default, uses the
#'  earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape
#'  diversity, formatted as "HH:MM:SS". If set to default, uses the
#'  latest time for which data exists in the dataframe.
#' @param twilight A character string of the twilight method to be used
#'  for sunrise and sunset as the boundary between day and night.
#' Options can be found in the \code{\link[photobiology]{day_night}}
#'  documentation.
#' @param dawnstart A numeric argument. If subset == 'dawn', used to determine
#' the start of dawn. By default, dawn starts at sunrise. Expressed as
#' the time in seconds before sunrise.
#' @param dawnend A numeric argument. If subset == 'dawn', used to determine
#' the end of dawn. By default, dawn ends 1.5 hours after sunrise.
#' Expressed as the time in seconds after sunrise.
#' @param duskstart A numeric argument. If subset == 'dusk', used to determine
#' the start of dusk. By default, dusk starts 1.5 hours before sunset.
#'  Expressed as the time in seconds before sunset.
#' @param duskend A numeric argument. If subset == 'dusk', used to determine the
#'  end of dusk. By default, dusk ends at sunset. Expressed as the
#'  time in seconds after sunset.
#' @param freqseq A logical operator (TRUE/FALSE). If set to FALSE, will
#'  compute the diversity for the entire frequency range of the soundscape.
#'  If set to TRUE, will compute the diversity per frequency-bin of
#'   user-defined width (number of bins determined by nbins argument).
#' @param nbins A numeric argument. If freqseq is set to TRUE, determines
#' the number of the frequency-bins by which to divide the frequency range
#' to compute the soundscape diversity.
#' @param output A character string. Indicates the format in which
#' the soundscape diversity is expressed. Options are "percentage"
#' (the fraction between the observed soundscape diversity and the
#' maximum possible soundscape diversity), or "raw" (the number of
#' acoustically active OSUs in the soundscape). Defaults to "percentage".
#' @return Depending on the chosen parameters, returns the soundscape
#' diversity either a numeric value, a vector of values or a list of
#' vectors of values.
#' @export
sounddiv=function(aggregated_soundscape,
                  qvalue,
                  subset="total",
                  mintime="default",
                  maxtime="default",
                  minfreq=0,
                  maxfreq="default",
                  twilight="sunlight",
                  dawnstart=0,
                  dawnend=5400,
                  duskstart=5400,
                  duskend=0,
                  freqseq=FALSE,
                  nbins=10,
                  output="percentage"){

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(aggregated_soundscape))
  assertthat::assert_that(test_0(qvalue))

  # 1. Check if function input meets expectations

  # 1.1. The supplied aggregated_soundscape argument is an S4-object of the type
  # 'soundscape', and is not empty.

  test_1 <- function(x){

    isS4(x) &
      assertthat::are_equal(class(x)[1], "soundscape") &
      assertthat::not_empty(x)

  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " is not an S4-object of the type 'soundscape', or is empty. Please supply the aggregated_soundscape object produced by the aggregate_df() function. Consult the package documentation for further information.")

  }

  assertthat::assert_that(test_1(aggregated_soundscape))

  # 1.2. The aggregated_soundscape elements are in the expected format

  # 1.2.1. The first_day argument cannot be wrong (S4 property)

  # 1.2.2. The lat and lon argument

  test_3 <- function(x){

    is.numeric(x) &
      x >= -90 &
      x <= 90

  }

  test_4 <- function(x){

    is.numeric(x) &
      x >= -180 &
      x <= 180

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_4) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_3(aggregated_soundscape@lat))
  assertthat::assert_that(test_4(aggregated_soundscape@lon))

  # 1.2.3. The time zone argument

  test_5 <- function(x){

    assertthat::is.string(x) & (x %in% (OlsonNames()))

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")

  }

  assertthat::assert_that(test_5(aggregated_soundscape@tz))

  # 1.2.4. The sunrise and sunset arguments cannot be wrong (s4 property)

  # 1.2.5. The fileloc argument

  test_6 <- function(x){

    assertthat::is.dir(x) & assertthat::is.readable(x)

  }

  assertthat::assert_that(test_6(aggregated_soundscape@fileloc))

  # 1.2.6. The index argument

  test_7 <- function(x){

    assertthat::is.string(x) & (x %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
                                         "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"))

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available index options. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")

  }

  assertthat::assert_that(test_7(aggregated_soundscape@index))

  # 1.2.7. The samplerate and window arguments

  test_8 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the samplerate and window arguments.")

  }

  assertthat::assert_that(test_8(aggregated_soundscape@samplerate))
  assertthat::assert_that(test_8(aggregated_soundscape@window))

  # 1.2.8. The binarization_method argument

  test_9 <- function(x){
    assertthat::is.string(x) & (x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li","MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu","Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode", "custom"))
  }



  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_9(aggregated_soundscape@binarization_method))

  # 1.2.9. The threshold argument

  test_10 <- function(x){

    all(length(x) == 1 &
          is.double(x) & !is.na(x))

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not a single numeric value. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the value argument is you're supplying a custom threshold value.")

  }

  assertthat::assert_that(test_10(aggregated_soundscape@threshold))

  # 1.2.10. The output argument

  test_11 <- function(x){

    all(length(x) == 1 & is.character(x) & (x %in% c("incidence_freq", "raw")))

  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not a character string describing one of the available output options. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.")

  }

  assertthat::assert_that(test_11(aggregated_soundscape@output))

  # 1.2.11. The merged_df argument

  test_12 <- function(x){

    is.data.frame(x) &
      assertthat::not_empty(x) &
      assertthat::noNA(x) &
      limma::isNumeric(x)

  }

  test_13 <- function(x){

    (abs(as.numeric(rownames(x)[1]))+
       abs(as.numeric(rownames(x)[2])))>3 &
      min(as.numeric(rownames(x))) >= 0 &
      max(as.numeric(rownames(x)))<= aggregated_soundscape@samplerate/2

  }

  test_14 <- function(x){

    formatted <-  try(
      as.POSIXct(
        paste0(substr(aggregated_soundscape@first_day, 1, 12)," ", colnames(x)),
        tz = aggregated_soundscape@tz,
        format="%Y-%m-%d %H:%M:%S"),
      silent = TRUE)

    !any(sapply(formatted, function(y) is.na(y)))

  }


  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.")

  }

  assertthat::on_failure(test_13) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.")

  }

  assertthat::on_failure(test_14) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.")

  }

  assertthat::assert_that(test_12(aggregated_soundscape@merged_df))
  assertthat::assert_that(test_13(aggregated_soundscape@merged_df))
  assertthat::assert_that(test_14(aggregated_soundscape@merged_df))

  # 1.2.12. The binarized_df argument

  test_15 <- function(x){

    min(x) >= 0 &
      max(x) <= 1

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the aggregate_df() function.")

  }

  assertthat::assert_that(test_12(aggregated_soundscape@binarized_df))
  assertthat::assert_that(test_13(aggregated_soundscape@binarized_df))
  assertthat::assert_that(test_14(aggregated_soundscape@binarized_df))
  assertthat::assert_that(test_15(aggregated_soundscape@binarized_df))

  # 1.2.12. The aggregated_df argument

  assertthat::assert_that(test_12(aggregated_soundscape@aggregated_df))
  assertthat::assert_that(test_13(aggregated_soundscape@aggregated_df))
  assertthat::assert_that(test_14(aggregated_soundscape@aggregated_df))

  if(aggregated_soundscape@output=="incidence_freq"){

    test_16 <- function(x){

      all(is.double(unlist(x)) & max(x) <= 1 & min(x)>= 0)

    }

    assertthat::on_failure(test_16) <- function(call, env){

      paste0(deparse(call$x), " contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument.")

    }

    assertthat::assert_that(test_16(aggregated_soundscape@aggregated_df))
  }

  if(aggregated_soundscape@output=="raw"){

    test_16 <- function(x){

      all(all(round(unlist(x)) == unlist(x)) &
            max(x) <= max(table(colnames(aggregated_soundscape@merged_df))) &
            min(x) >= 0)

    }

    assertthat::on_failure(test_16) <- function(call, env){

      paste0(deparse(call$x), " contains values smaller than zero, or larger than the maximum number of soundscape samples per time. The expected range of raw values ranges between 0 and the maximum number of soundscape samples (24-hour recording days). Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument.")

    }

    assertthat::assert_that(test_16(aggregated_soundscape@aggregated_df))
  }

  # 1.2.13. The aggregated_df_per_time argument

  test_17_1 <- function(x){

    all(sapply(x, function(x) is.data.frame(x))) &
      all(assertthat::are_equal(
        as.vector(sort(table(colnames(aggregated_soundscape@merged_df)))),
        as.vector(unlist(sapply(x, function(x) ncol(x))))
      )) &
      length(x) == ncol(aggregated_soundscape@aggregated_df)
  }

  test_17_2 <- function(x){

    all(sapply(x, function(x) nrow(x)==nrow(aggregated_soundscape@merged_df)))

  }


  assertthat::on_failure(test_17_1) <- function(call, env){

    paste0(deparse(call$x), " does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.")

  }

  assertthat::on_failure(test_17_2) <- function(call, env){

    paste0(deparse(call$x), " does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.")

  }

  assertthat::assert_that(test_17_1(aggregated_soundscape@aggregated_df_per_time))
  assertthat::assert_that(test_17_2(aggregated_soundscape@aggregated_df_per_time))

  # 1.2.14. The effort_per_time argument

  test_18 <- function(x){

    identical(as.list(sort(table(colnames(aggregated_soundscape@merged_df)))), x)

  }

  assertthat::on_failure(test_18) <- function(call, env){

    paste0(deparse(call$x), " does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.")

  }

  assertthat::assert_that(test_18(aggregated_soundscape@effort_per_time))

  # 1.3. The supplied qvalue argument is a positive integer or decimal number

  test_5 <- function(x){
    !is.character(x)
  }

  test_6 <- function(x){
    !is.list(x)
  }

  test_7 <- function(x){

    is.numeric(x)

  }

  test_8 <- function(x){

    abs(x) == x

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is a character string of length 1. Please supply the qvalue argument as a positive numeric or integer value.")

  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " is a list. Please supply the qvalue argument as a positive numeric or integer value.")

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not an numeric/integer value. Please supply the qvalue argument as a positive numeric or integer value.")

  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " is not a positive value. Please supply the qvalue argument as a positive numeric or integer value.")

  }

  assertthat::assert_that(test_5(qvalue))
  assertthat::assert_that(test_6(qvalue))
  assertthat::assert_that(test_7(qvalue))
  assertthat::assert_that(test_8(qvalue))

  # 1.4. The subset argument is a character string, and one of the
  # available options

  test_9 <- function(x){
    assertthat::is.string(x)
  }

  test_10 <- function(x){
    x %in% c("total","day", "night", "dawn", "dusk", "tod")
  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the sounddiv subset argument as a character string. Consult package documentation for available subset argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available sounddiv subset options. Please consult package documentation for available subset argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_9(subset))
  assertthat::assert_that(test_10(subset))


  # 1.5. the supplied mintime and maxtime arguments
  # are one of the available options

  test_15 <- function(x){

    is.character(x) & (

      x == "default" |
        !is.na(as.POSIXct(x, format="%H:%M:%S")))

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.")

  }

  assertthat::assert_that(test_15(mintime))
  assertthat::assert_that(test_15(maxtime))

  # 1.6. The minfreq and maxfreq arguments follow
  # the expected values

  test_16 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregated_soundscape@aggregated_df))) &
       x <= max(as.numeric(rownames(aggregated_soundscape@aggregated_df)))) |
      x == 0

  }

  test_17 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregated_soundscape@aggregated_df))) &
       x <= max(as.numeric(rownames(aggregated_soundscape@aggregated_df)))) |
      x == "default"

  }

  assertthat::on_failure(test_16) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.")

  }

  assertthat::on_failure(test_17) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.")

  }

  assertthat::assert_that(test_16(minfreq))
  assertthat::assert_that(test_17(maxfreq))

  # 1.7. The supplied twilight argument is one
  # of the available options

  test_18 <- function(x){

    (assertthat::is.string(x) &
       x %in% c("none","rim","refraction","sunlight","civil",
                "nautical","astronomical")) |
      (is.vector(x, mode="double") & (length(x) == 1 | length(x) ==2))

  }

  assertthat::on_failure(test_18) <- function(call, env){

    paste0(deparse(call$x), " is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.")

  }

  assertthat::assert_that(test_18(twilight))

  # 1.8. The dawnstart, dawnend, duskstart and duskend arguments are either
  # zero or a single positive integer

  test_19 <- function(x){

    x == 0 | assertthat::is.count(x)

  }

  assertthat::on_failure(test_19) <- function(call, env){

    paste0(deparse(call$x), " is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.")

  }

  assertthat::assert_that(test_19(dawnstart))
  assertthat::assert_that(test_19(dawnend))
  assertthat::assert_that(test_19(duskstart))
  assertthat::assert_that(test_19(duskend))

  # 1.9. The freqseq argument is a boolean flag

  test_20 <- function(x){

    assertthat::is.flag(x)

  }

  assertthat::on_failure(test_20) <- function(call, env){

    paste0(deparse(call$x), " is not a Boolean flag (TRUE or FALSE). Please set the freqseq argument to TRUE or FALSE. Make sure the argument is not a character string.")

  }

  assertthat::assert_that(test_20(freqseq))

  # 1.10. Check if the nbins argument abides by the
  # expected format

  test_21 <- function(x){

    assertthat::is.count(x) &
      x > 0 &
      x < nrow(aggregated_soundscape@aggregated_df)
  }

  assertthat::on_failure(test_21) <- function(call, env){

    paste0(deparse(call$x), " is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.")

  }

  assertthat::assert_that(test_21(nbins))

  # 1.11. The output argument is a string and one of the available options

  test_22 <- function(x){
    assertthat::is.string(x)
  }

  test_23 <- function(x){
    x %in% c("percentage", "raw")
  }

  assertthat::on_failure(test_22) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the output argument as a character string. Consult package documentation for available output argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_23) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available sounddiv output options. Please consult package documentation for available output argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_22(output))
  assertthat::assert_that(test_23(output))

  # 2. Create a multiplier variable to multiply the OSU values by for
  # different output options

  if (output=="raw"){
    multiplier <- 1}

  else{

    if(output=="percentage"){
      multiplier <- 100}

  }

  # 3. Create the diurnal phase subsetting objects

  tz <- aggregated_soundscape@tz

  day <- as.POSIXct(
    strptime(
      paste(substr(aggregated_soundscape@first_day, 1, 12),
            "00:00:00",
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))

  sunrise <- aggregated_soundscape@sunrise

  sunset <- aggregated_soundscape@sunset

  # 4. Set minfreq, maxfreq, mintime and maxtime arguments

  if (maxfreq=="default"){
    maxfreq <- max(
      as.numeric(
        rownames(aggregated_soundscape@aggregated_df)))
  }

  else{maxfreq <- maxfreq}

  if (mintime=="default"){
    mintime <- min(
      as.POSIXct(
        strptime(
          paste(
            substr(aggregated_soundscape@first_day, 1, 12),
            colnames(aggregated_soundscape@aggregated_df),
            sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz=tz)))
  }

  else{mintime <- as.POSIXct(
    strptime(
      paste(
        substr(aggregated_soundscape@first_day, 1, 12),
        mintime,
        sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))}


  if (maxtime=="default"){
    maxtime <- max(
      as.POSIXct(
        strptime(
          paste(substr(aggregated_soundscape@first_day, 1, 12),
                colnames(aggregated_soundscape@aggregated_df),
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz=tz)))
  }

  else{maxtime <- as.POSIXct(
    strptime(
      paste(
        substr(aggregated_soundscape@first_day, 1, 12),
        maxtime,
        sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))}

  # 5. Set row names, column names and subsetting objects + create new df

  rownames_df <- as.numeric(
    rownames(aggregated_soundscape@aggregated_df))

  rownames_subset <- as.character(
    subset(rownames_df,
           rownames_df >= minfreq &
             rownames_df <= maxfreq))

  colnames_df <- as.POSIXct(
    strptime(
      paste(substr(aggregated_soundscape@first_day, 1, 12),
            colnames(aggregated_soundscape@aggregated_df),
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))

  colnames_subset <- as.character(
    hms::as_hms(
      subset(colnames_df,
             colnames_df >= mintime &
               colnames_df <= maxtime)))

  new_df <- aggregated_soundscape@aggregated_df[rownames_subset,colnames_subset]

  # Compute the soundscape diversity under different scenarios

  if (freqseq=="FALSE"){

    if (subset == "total"){

      soundscape_diversity <-
        hilldiv::hill_div(unlist(new_df),
                          qvalue=qvalue) / if (output=="raw"){1}
      else{
        if(output=="percentage"){
          (ncol(new_df)*nrow(new_df))
        }
      }

      soundscape_diversity <- soundscape_diversity*multiplier

      soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

      soundscape_diversity

    }
    else{

      if (subset == "tod"){

        soundscape_diversity <- c()

        for (i in 1:ncol(new_df)){
          soundscape_diversity[i] <-
            hilldiv::hill_div(unlist(new_df[[i]]),
                              qvalue = qvalue)/ if (output=="raw"){1}
          else{
            if(output=="percentage"){length(new_df[[i]])}
          }

          if(sum(unlist(new_df[[i]]))==0){
            soundscape_diversity[i] <- 0
          }
        }

        soundscape_diversity <- soundscape_diversity*multiplier

        soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

        soundscape_diversity <- as.data.frame(soundscape_diversity)

        soundscape_diversity$time <- hms::as_hms(
          (as.POSIXct(
            strptime(
              paste(substr(aggregated_soundscape@first_day, 1, 12),
                    colnames(aggregated_soundscape@aggregated_df),
                    sep=" "),
              format= "%Y-%m-%d %H:%M:%S",
              tz=tz))))

        colnames(soundscape_diversity) <- c("soundscape_div", "time_of_day")

        soundscape_diversity
      }

      else{

        if (subset == "day"){

          colnames_day <-
            as.character(
              hms::as_hms(
                subset(colnames_df,
                       colnames_df >= sunrise &
                         colnames_df <= sunset)))

          daytime_df <- aggregated_soundscape@aggregated_df[rownames_subset,colnames_day]

          soundscape_diversity <-
            hilldiv::hill_div(
              unlist(daytime_df),
              qvalue=qvalue) / if (output=="raw"){1}
          else{
            if(output=="percentage"){
              (ncol(daytime_df)*nrow(daytime_df))
            }
          }

          soundscape_diversity <- soundscape_diversity*multiplier

          soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

          soundscape_diversity

        }

        else{

          if(subset == "night"){

            colnames_night <-
              as.character(
                hms::as_hms(
                  subset(colnames_df,
                         colnames_df < sunrise |
                           colnames_df > sunset)))

            nighttime_df <- aggregated_soundscape@aggregated_df[rownames_subset,
                                                                colnames_night]

            soundscape_diversity <-
              hilldiv::hill_div(
                unlist(nighttime_df),
                qvalue=qvalue) / if (output=="raw"){1}
            else{
              if(output=="percentage"){
                (ncol(nighttime_df)*nrow(nighttime_df))
              }
            }

            soundscape_diversity <- soundscape_diversity*multiplier

            soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

            soundscape_diversity
          }

          else{

            if (subset == "dawn"){

              colnames_dawn <- as.character(
                hms::as_hms(
                  subset(colnames_df,
                         colnames_df >= (sunrise - dawnstart) &
                           colnames_df <= (sunrise + dawnend))))

              dawntime_df <- aggregated_soundscape@aggregated_df[rownames_subset,
                                                                 colnames_dawn]

              soundscape_diversity <-
                hilldiv::hill_div(
                  unlist(dawntime_df),
                  qvalue=qvalue) / if (output=="raw"){1}
              else{
                if(output=="percentage"){
                  (ncol(dawntime_df)*nrow(dawntime_df))
                }
              }

              soundscape_diversity <- soundscape_diversity*multiplier

              soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

              soundscape_diversity

            }

            else{

              if (subset == "dusk"){

                colnames_dusk <- as.character(
                  hms::as_hms(
                    subset(colnames_df,
                           colnames_df >= (sunset - duskstart) &
                             colnames_df <= (sunset + duskend))))

                dusktime_df <- aggregated_soundscape@aggregated_df[rownames_subset,
                                                                   colnames_dusk]

                soundscape_diversity <-
                  hilldiv::hill_div(
                    unlist(dusktime_df),
                    qvalue=qvalue) / if (output=="raw"){1}
                else{
                  if(output=="percentage"){
                    (ncol(dusktime_df)*nrow(dusktime_df))
                  }
                }

                soundscape_diversity <- soundscape_diversity*multiplier

                soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

                soundscape_diversity

              }
            }
          }
        }
      }
    }
  }

  else{

    if (freqseq=="TRUE"){

      new_df$frequency <- as.numeric(rownames(new_df))
      minfreq_label <- min(new_df$frequency)
      maxfreq_label <- max(new_df$frequency)
      rownames <- as.numeric(rownames(new_df))

      freq_list_1 <- vector("list", 0)
      freq_list_2 <- vector("list", 0)
      rownames_1 <- vector("list", 0)
      rownames_2 <- vector("list", 0)


      for (i in 1:nbins){

        freq_list_1[[i]] <- subset(new_df,
                                   new_df$frequency < (i*(maxfreq/nbins)))

        rownames_1[[i]] <- subset(rownames,
                                  rownames < (i*(maxfreq/nbins)))

      }

      for (i in 1:nbins){

        freq_list_2[[i]] <-
          subset(freq_list_1[[i]],
                 freq_list_1[[i]]$frequency > ((i-1)*(maxfreq/nbins)))

        freq_list_2[[i]]$frequency <- NULL

        rownames_2[[i]] <- as.character(
          subset(rownames_1[[i]],
                 rownames_1[[i]] > ((i-1)*(maxfreq/nbins))))

      }

      binnames_min <- vector("list", 0)
      binnames_max <- vector("list", 0)
      binnames_tot <- vector("list", 0)

      for (i in 1:length(freq_list_2)){

        binnames_min[[i]] <- min(as.numeric(rownames(freq_list_2[[i]]))) -
          as.integer(aggregated_soundscape@samplerate / aggregated_soundscape@window)
        binnames_max[[i]] <- max(as.numeric(rownames(freq_list_2[[i]])))
        binnames_tot[[i]] <- paste0(binnames_min[[i]], " - ", binnames_max[[i]], " Hz")
      }

      binnames_tot <- unlist(binnames_tot)

      if (subset == "total"){

        soundscape_diversity <- c()

        for (i in 1:length(freq_list_2)){

          soundscape_diversity[i] <-
            hilldiv::hill_div(
              unlist(freq_list_2[[i]]),
              qvalue=qvalue) / if (output=="raw"){1}

          else{
            if(output=="percentage"){
              (ncol(freq_list_2[[i]])*nrow(freq_list_2[[i]]))
            }
          }
        }

        for (i in 1:length(freq_list_2)){

          if (sum(unlist(freq_list_2[i]))==0){
            soundscape_diversity[i] <- 0
          }

          else{
            soundscape_diversity[i] <- soundscape_diversity[i]
          }
        }

        soundscape_diversity <- soundscape_diversity*multiplier
        soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

        soundscape_diversity <- as.data.frame(soundscape_diversity)

        soundscape_diversity$frequency_bin <- binnames_tot

        colnames(soundscape_diversity) <- c("soundscape_div", "freq_interval")

        soundscape_diversity
      }


      else{

        if (subset == "tod"){

          soundscape_diversity <- vector("list",
                                         length(freq_list_2))

          for (i in 1:length(freq_list_2)){

            soundscape_diversity[[i]] <- vector("list",
                                                ncol(freq_list_2[[i]]))

            for (j in 1:ncol(freq_list_2[[i]])){

              soundscape_diversity[[i]][[j]] <-
                hilldiv::hill_div(
                  unlist(freq_list_2[[i]][[j]]),
                  qvalue = qvalue) / if (output=="raw"){1}
              else{
                if(output=="percentage"){
                  length(freq_list_2[[i]][[j]])
                }
              }
            }
          }

          soundscape_diversity

          for (i in 1:length(freq_list_2)){

            for (j in 1:ncol(freq_list_2[[i]])){

              if (all.equal(rep(0,length(freq_list_2[[i]][[j]])),
                            freq_list_2[[i]][[j]])==TRUE){

                soundscape_diversity[[i]][[j]] <- 0

              }

              else{

                soundscape_diversity[[i]][[j]] <- soundscape_diversity[[i]][[j]]

              }
            }
          }

          for (i in 1:length(soundscape_diversity)){

            soundscape_diversity[[i]] <- unlist(soundscape_diversity[[i]])

          }

          for (i in 1:length(soundscape_diversity)){

            soundscape_diversity[[i]] <- soundscape_diversity[[i]]*multiplier

            soundscape_diversity[[i]][!is.finite(soundscape_diversity[[i]])] <- 0
            soundscape_diversity[[i]] <- as.data.frame(
              soundscape_diversity[[i]])

            soundscape_diversity[[i]]$time <-
              hms::as_hms(
                (as.POSIXct(
                  strptime(
                    paste(substr(aggregated_soundscape@first_day, 1, 12),
                          colnames(aggregated_soundscape@aggregated_df),
                          sep=" "),
                    format= "%Y-%m-%d %H:%M:%S",
                    tz=tz))))

            colnames(soundscape_diversity[[i]]) <- c("soundscape_div", "time_of_day")
          }

          names(soundscape_diversity) <- binnames_tot

          soundscape_diversity

        }

        else{

          if (subset == "day"){

            colnames_day <- as.character(
              hms::as_hms(
                subset(colnames_df,
                       colnames_df >= sunrise &
                         colnames_df <= sunset)))

            freq_list_day <- vector("list", 0)

            for (i in 1:length(freq_list_2)){

              freq_list_day[[i]] <- freq_list_2[[i]][rownames_2[[i]],
                                                     colnames_day]
            }

            soundscape_diversity <- c()

            for (i in 1:length(freq_list_day)){

              soundscape_diversity[i] <-
                hilldiv::hill_div(
                  unlist(freq_list_day[[i]]),
                  qvalue=qvalue) / if (output=="raw"){1}

              else{

                if(output=="percentage"){
                  (ncol(freq_list_day[[i]])*nrow(freq_list_day[[i]]))
                }
              }
            }

            for (i in 1:length(freq_list_day)){

              if (identical(rep(0, length(freq_list_day[[i]])),
                            freq_list_day[[i]])){
                soundscape_diversity[[i]] <- 0
              }

              else{
                soundscape_diversity[[i]] <- soundscape_diversity[[i]]
              }
            }

            soundscape_diversity <- soundscape_diversity*multiplier
            soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

            soundscape_diversity <- as.data.frame(soundscape_diversity)

            soundscape_diversity$frequency_bin <-  binnames_tot

            colnames(soundscape_diversity) <- c("soundscape_div", "freq_interval")

            soundscape_diversity

          }

          else{

            if (subset == "night"){

              colnames_night <- as.character(
                hms::as_hms(
                  subset(colnames_df,
                         colnames_df <= sunrise |
                           colnames_df >= sunset)))

              freq_list_night <- vector("list", 0)

              for (i in 1:length(freq_list_2)){

                freq_list_night[[i]] <- freq_list_2[[i]][rownames_2[[i]],
                                                         colnames_night]

              }

              soundscape_diversity <- c()

              for (i in 1:length(freq_list_night)){

                soundscape_diversity[i] <-
                  hilldiv::hill_div(
                    unlist(freq_list_night[[i]]),
                    qvalue=qvalue) / if (output=="raw"){1}

                else{

                  if(output=="percentage"){

                    (ncol(freq_list_night[[i]])*nrow(freq_list_night[[i]]))

                  }
                }
              }

              for (i in 1:length(freq_list_night)){

                if (identical(rep(0, length(freq_list_night[[i]])),
                              freq_list_night[[i]])){
                  soundscape_diversity[[i]] <- 0
                }

                else{
                  soundscape_diversity[[i]] <- soundscape_diversity[[i]]
                }
              }

              soundscape_diversity <- soundscape_diversity*multiplier
              soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

              soundscape_diversity <- as.data.frame(soundscape_diversity)

              soundscape_diversity$frequency_bin <- binnames_tot

              colnames(soundscape_diversity) <- c("soundscape_div", "freq_interval")

              soundscape_diversity

            }

            else{

              if (subset == "dawn"){

                colnames_dawn <- as.character(
                  hms::as_hms(
                    subset(colnames_df,
                           colnames_df > (sunrise-dawnstart) &
                             colnames_df <= (sunrise+dawnend))))

                freq_list_dawn <- vector("list", 0)

                for (i in 1:length(freq_list_2)){
                  freq_list_dawn[[i]] <- freq_list_2[[i]][rownames_2[[i]],
                                                          colnames_dawn]
                }

                soundscape_diversity <- c()

                for (i in 1:length(freq_list_dawn)){

                  soundscape_diversity[i] <-
                    hilldiv::hill_div(
                      unlist(freq_list_dawn[[i]]),
                      qvalue=qvalue) / if (output=="raw"){1}

                  else{

                    if(output=="percentage"){

                      (ncol(freq_list_dawn[[i]])*nrow(freq_list_dawn[[i]]))

                    }
                  }
                }

                for (i in 1:length(freq_list_dawn)){

                  if (identical(rep(0, length(freq_list_dawn[[i]])),
                                freq_list_dawn[[i]])==TRUE){
                    soundscape_diversity[[i]] <- 0
                  }

                  else{
                    soundscape_diversity[[i]] <- soundscape_diversity[[i]]
                  }
                }

                soundscape_diversity <- soundscape_diversity*multiplier

                soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

                soundscape_diversity <- as.data.frame(soundscape_diversity)

                soundscape_diversity$frequency_bin <- binnames_tot

                colnames(soundscape_diversity) <- c("soundscape_div", "freq_interval")

                soundscape_diversity

              }


              else{

                if (subset == "dusk"){

                  colnames_dusk <- as.character(
                    hms::as_hms(
                      subset(colnames_df,
                             colnames_df > (sunset - duskstart) &
                               colnames_df <= (sunset + duskend))))

                  freq_list_dusk <- vector("list", 0)

                  for (i in 1:length(freq_list_2)){
                    freq_list_dusk[[i]] <- freq_list_2[[i]][rownames_2[[i]],
                                                            colnames_dusk]
                  }


                  soundscape_diversity <- c()

                  for (i in 1:length(freq_list_dusk)){

                    soundscape_diversity[i] <-
                      hilldiv::hill_div(
                        unlist(freq_list_dusk[[i]]),
                        qvalue=qvalue) / if (output=="raw"){1}

                    else{

                      if(output=="percentage"){

                        (ncol(freq_list_dusk[[i]])*nrow(freq_list_dusk[[i]]))

                      }
                    }
                  }
                }

                for (i in 1:length(freq_list_dusk)){

                  if (identical(rep(0, length(freq_list_dusk[[i]])),
                                freq_list_dusk[[i]])==TRUE){
                    soundscape_diversity[[i]] <- 0
                  }

                  else{
                    soundscape_diversity[[i]] <- soundscape_diversity[[i]]
                  }
                }

                soundscape_diversity <- soundscape_diversity*multiplier

                soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
                soundscape_diversity <- as.data.frame(soundscape_diversity)

                soundscape_diversity$frequency_bin <- binnames_tot

                colnames(soundscape_diversity) <- c("soundscape_div", "freq_interval")

                soundscape_diversity

              }
            }
          }
        }
      }
    }
  }
}


# Functions for visualizing soundscape diversity by time of day

#1) Plots showing soundscape diversity by time of day

utils::globalVariables(c("time_of_day", "soundscape_div", "frequency", "freq", "soundscape_div_smooth", "prop_sound_div"))

#' Visualize Soundscape Diversity by Time of Day
#'
#' @description Produces plots showing the variation in soundscape diversity by time-of-day. Soundscape diversity can be shown for the full frequency range, or the relative contribution of frequency-bins with user-specified width.
#'
#' @param aggregated_soundscape The aggregated soundscape object produced by
#'  \code{\link{aggregate_df}} function.
#' @param qvalue A positive integer or decimal number (>=0), most commonly between 0-3. This parameter modulates the sensitivity of diversity values to the relative abundance of Operational Sound Units (OSUs). A value of 0 corresponds to the richness, a value of 1 is the equivalent number of effective OSUs for the Shannon index, a value of 2 is the equivalent number of effective OSUs for the Simpson index.
#' @param graphtype The type of plot which is produced.
#'
#'Options are:
#'
#'\emph{'total'}:
#'
#'An area chart showing the soundscape diversity by time-of-day for the entire frequency range.
#'
#'\emph{'frequency'}:
#'
#'A stacked area chart showing the relative contribution of frequency bins with user-defined width to the total soundscape diversity by time-of-day.
#'
#'\emph{'normfreq'}:
#'
#'A percentage stacked area chart showing the normalized relative contribution of frequency bins with user-defined width to the soundscape diversity by time-of-day.
#'
#'\emph{'linefreq'}:
#'
#'A line chart showing the relative contribution of frequency bins with user-defined width to the soundscape diversity by time-of-day.
#' @param minfreq The lower frequency limit for which to visualize the soundscape diversity, expressed as a numeric value.
#' Defaults to the lowest frequency for which data exists in the dataframe.
#' @param maxfreq The upper frequency limit for which to visualize the soundscape diversity, expressed as a numeric value.
#' Defaults to the highest frequency for which data exists in the dataframe.
#' @param nbins If graphtype='frequency'/'normfreq'/'linefreq', determines the number of the frequency-bins by which to divide the frequency range to compute the
#' relative contribution of each bin to the total diversity.
#' @param timeinterval A time interval for the x-axis. Options can be found in the \code{\link[scales]{date_breaks}} documentation.
#' @param smooth One of either TRUE or FALSE. If set to TRUE, applies a moving average filter for smoothing the diversity by time-of-day.
#' @param movavg If smooth=TRUE, determines the width of the moving average filter. Consult \code{\link[pracma]{movavg}} for more information.
#' @param interactive One of either TRUE or FALSE. If set to TRUE, an interactive plot is produced using \code{\link[plotly]{ggplotly}}.
#' @param save One of either TRUE or FALSE. If set to TRUE, saves the plot using \code{\link[ggplot2]{ggsave}}, and the 'dir', 'filename' and 'device'
#' arguments.
#' @param dir Path of the directory to save plot to: path and filename are combined to create the fully qualified file name.
#' Defaults to the working directory. For more information consult \code{\link[ggplot2]{ggsave}}.
#' @param filename The file name without the extention. For more information consult \code{\link[ggplot2]{ggsave}}.
#' @param device Device to use. Can either be a device function (e.g. png()), or one of
#' "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' Defaults to "png". For more information consult \code{\link[ggplot2]{ggsave}}.
#' @param output A character string. Indicates the format in which the soundscape diversity is expressed. Options are "percentage" (the fraction between the observed soundscape diversity and the maximum possible soundscape diversity), or "raw" (the number of acoustically active OSUs in the soundscape). Defaults to "percentage".
#'
#'@param width If save=TRUE, expresses the width of the saved image in milimeters. Defaults to 100 mm.
#' @param height If save=TRUE, expresses the height of the saved image in milimeters. Defaults to 100 mm.
#'
#' @return Returns a ggplot object and if save=TRUE, saves the plot in a directory of choice using a specified device and filename.
#' @export
sounddiv_by_time=function(aggregated_soundscape,
                          qvalue,
                          graphtype = "total",
                          minfreq = 0,
                          maxfreq = "default",
                          nbins = 10,
                          timeinterval = "1 hour",
                          smooth = TRUE,
                          movavg = 6,
                          interactive = FALSE,
                          save = FALSE,
                          dir = "default",
                          filename = "file",
                          device = "png",
                          output = "percentage",
                          width = 150,
                          height = 150){

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(aggregated_soundscape))
  assertthat::assert_that(test_0(qvalue))

  # 1. Check if function input meets expectations

  # 1.1. The supplied aggregated_soundscape argument is an S4-object of the type
  # 'soundscape', and is not empty.

  test_1 <- function(x){

    isS4(x) &
      assertthat::are_equal(class(x)[1], "soundscape") &
      assertthat::not_empty(x)

  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " is not an S4-object of the type 'soundscape', or is empty. Please supply the aggregated_soundscape object produced by the aggregate_df() function. Consult the package documentation for further information.")

  }

  assertthat::assert_that(test_1(aggregated_soundscape))

  # 1.2. The aggregated_soundscape elements are in the expected format

  # 1.2.1. The first_day argument cannot be wrong (S4 property)

  # 1.2.2. The lat and lon argument

  test_3 <- function(x){

    is.numeric(x) &
      x >= -90 &
      x <= 90

  }

  test_4 <- function(x){

    is.numeric(x) &
      x >= -180 &
      x <= 180

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_4) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_3(aggregated_soundscape@lat))
  assertthat::assert_that(test_4(aggregated_soundscape@lon))

  # 1.2.3. The time zone argument

  test_5 <- function(x){

    assertthat::is.string(x) & (x %in% (OlsonNames()))

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")

  }

  assertthat::assert_that(test_5(aggregated_soundscape@tz))

  # 1.2.4. The sunrise and sunset arguments cannot be wrong (s4 property)

  # 1.2.5. The fileloc argument

  test_6 <- function(x){

    assertthat::is.dir(x) & assertthat::is.readable(x)

  }

  assertthat::assert_that(test_6(aggregated_soundscape@fileloc))

  # 1.2.6. The index argument

  test_7 <- function(x){

    assertthat::is.string(x) & (x %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
                                         "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"))

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available index options. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")

  }

  assertthat::assert_that(test_7(aggregated_soundscape@index))

  # 1.2.7. The samplerate and window arguments

  test_8 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the samplerate and window arguments.")

  }

  assertthat::assert_that(test_8(aggregated_soundscape@samplerate))
  assertthat::assert_that(test_8(aggregated_soundscape@window))

  # 1.2.8. The binarization_method argument

  test_9 <- function(x){
    assertthat::is.string(x) & (x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li","MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu","Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode", "custom"))
  }



  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_9(aggregated_soundscape@binarization_method))

  # 1.2.9. The threshold argument

  test_10 <- function(x){

    all(length(x) == 1 &
          is.double(x) & !is.na(x))

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not a single numeric value. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the value argument is you're supplying a custom threshold value.")

  }

  assertthat::assert_that(test_10(aggregated_soundscape@threshold))

  # 1.2.10. The output argument

  test_11 <- function(x){

    all(length(x) == 1 & is.character(x) & (x %in% c("incidence_freq", "raw")))

  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not a character string describing one of the available output options. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.")

  }

  assertthat::assert_that(test_11(aggregated_soundscape@output))

  # 1.2.11. The merged_df argument

  test_12 <- function(x){

    is.data.frame(x) &
      assertthat::not_empty(x) &
      assertthat::noNA(x) &
      limma::isNumeric(x)

  }

  test_13 <- function(x){

    (abs(as.numeric(rownames(x)[1]))+
       abs(as.numeric(rownames(x)[2])))>3 &
      min(as.numeric(rownames(x))) >= 0 &
      max(as.numeric(rownames(x)))<= aggregated_soundscape@samplerate/2

  }

  test_14 <- function(x){

    formatted <-  try(
      as.POSIXct(
        paste0(substr(aggregated_soundscape@first_day, 1, 12)," ", colnames(x)),
        tz = aggregated_soundscape@tz,
        format="%Y-%m-%d %H:%M:%S"),
      silent = TRUE)

    !any(sapply(formatted, function(y) is.na(y)))

  }


  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.")

  }

  assertthat::on_failure(test_13) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.")

  }

  assertthat::on_failure(test_14) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.")

  }

  assertthat::assert_that(test_12(aggregated_soundscape@merged_df))
  assertthat::assert_that(test_13(aggregated_soundscape@merged_df))
  assertthat::assert_that(test_14(aggregated_soundscape@merged_df))

  # 1.2.12. The binarized_df argument

  test_15 <- function(x){

    min(x) >= 0 &
      max(x) <= 1

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the aggregate_df() function.")

  }

  assertthat::assert_that(test_12(aggregated_soundscape@binarized_df))
  assertthat::assert_that(test_13(aggregated_soundscape@binarized_df))
  assertthat::assert_that(test_14(aggregated_soundscape@binarized_df))
  assertthat::assert_that(test_15(aggregated_soundscape@binarized_df))

  # 1.2.12. The aggregated_df argument

  assertthat::assert_that(test_12(aggregated_soundscape@aggregated_df))
  assertthat::assert_that(test_13(aggregated_soundscape@aggregated_df))
  assertthat::assert_that(test_14(aggregated_soundscape@aggregated_df))

  if(aggregated_soundscape@output=="incidence_freq"){

    test_16 <- function(x){

      all(is.double(unlist(x)) & max(x) <= 1 & min(x)>= 0)

    }

    assertthat::on_failure(test_16) <- function(call, env){

      paste0(deparse(call$x), " contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument.")

    }

    assertthat::assert_that(test_16(aggregated_soundscape@aggregated_df))
  }

  if(aggregated_soundscape@output=="raw"){

    test_16 <- function(x){

      all(all(round(unlist(x)) == unlist(x)) &
            max(x) <= max(table(colnames(aggregated_soundscape@merged_df))) &
            min(x) >= 0)

    }

    assertthat::on_failure(test_16) <- function(call, env){

      paste0(deparse(call$x), " contains values smaller than zero, or larger than the maximum number of soundscape samples per time. The expected range of raw values ranges between 0 and the maximum number of soundscape samples (24-hour recording days). Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument.")

    }

    assertthat::assert_that(test_16(aggregated_soundscape@aggregated_df))
  }

  # 1.2.13. The aggregated_df_per_time argument

  test_17_1 <- function(x){

    all(sapply(x, function(x) is.data.frame(x))) &
      all(assertthat::are_equal(
        as.vector(sort(table(colnames(aggregated_soundscape@merged_df)))),
        as.vector(unlist(sapply(x, function(x) ncol(x))))
      )) &
      length(x) == ncol(aggregated_soundscape@aggregated_df)
  }

  test_17_2 <- function(x){

    all(sapply(x, function(x) nrow(x)==nrow(aggregated_soundscape@merged_df)))

  }


  assertthat::on_failure(test_17_1) <- function(call, env){

    paste0(deparse(call$x), " does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.")

  }

  assertthat::on_failure(test_17_2) <- function(call, env){

    paste0(deparse(call$x), " does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.")

  }

  assertthat::assert_that(test_17_1(aggregated_soundscape@aggregated_df_per_time))
  assertthat::assert_that(test_17_2(aggregated_soundscape@aggregated_df_per_time))

  # 1.2.14. The effort_per_time argument

  test_18 <- function(x){

    identical(as.list(sort(table(colnames(aggregated_soundscape@merged_df)))), x)

  }

  assertthat::on_failure(test_18) <- function(call, env){

    paste0(deparse(call$x), " does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.")

  }

  assertthat::assert_that(test_18(aggregated_soundscape@effort_per_time))

  # 1.3. The supplied qvalue argument is a positive integer or decimal number

  test_5 <- function(x){
    !is.character(x)
  }

  test_6 <- function(x){
    !is.list(x)
  }

  test_7 <- function(x){

    is.numeric(x)

  }

  test_8 <- function(x){

    abs(x) == x

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is a character string of length 1. Please supply the qvalue argument as a positive numeric or integer value.")

  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " is a list. Please supply the qvalue argument as a positive numeric or integer value.")

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not an numeric/integer value. Please supply the qvalue argument as a positive numeric or integer value.")

  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " is not a positive value. Please supply the qvalue argument as a positive numeric or integer value.")

  }

  assertthat::assert_that(test_5(qvalue))
  assertthat::assert_that(test_6(qvalue))
  assertthat::assert_that(test_7(qvalue))
  assertthat::assert_that(test_8(qvalue))

  # 1.4. The graphtype argument is a character string, and one of the
  # available options

  test_9 <- function(x){
    assertthat::is.string(x)
  }

  test_10 <- function(x){
    x %in% c("total","frequency", "normfreq", "linefreq")
  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the sounddiv_by_time graphtype argument as a character string. Consult package documentation for available graphtype argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available sounddiv_by_time graphtype options. Please consult package documentation for available graphtype argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_9(graphtype))
  assertthat::assert_that(test_10(graphtype))

  # 1.5. The minfreq and maxfreq arguments follow
  # the expected values

  test_16 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregated_soundscape@aggregated_df))) &
       x <= max(as.numeric(rownames(aggregated_soundscape@aggregated_df)))) |
      x == 0

  }

  test_17 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregated_soundscape@aggregated_df))) &
       x <= max(as.numeric(rownames(aggregated_soundscape@aggregated_df)))) |
      x == "default"

  }

  assertthat::on_failure(test_16) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.")

  }

  assertthat::on_failure(test_17) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.")

  }

  assertthat::assert_that(test_16(minfreq))
  assertthat::assert_that(test_17(maxfreq))

  # 1.6. Check if the nbins argument abides by the
  # expected format

  test_21 <- function(x){

    assertthat::is.count(x) &
      x > 0 &
      x < nrow(aggregated_soundscape@aggregated_df)
  }

  assertthat::on_failure(test_21) <- function(call, env){

    paste0(deparse(call$x), " is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.")

  }

  assertthat::assert_that(test_21(nbins))

  # 1.7. Check if supplied timeinterval argument is one of
  # the available options

  test_22 <- function(x){

    any(stringr::str_detect(timeinterval,c("sec", "secs", "min", "mins", "hour", "hours", "day", "days", "week", "weeks", "month", "months", "year", "years"))) &
      grepl("^[[:digit:]]\\s", timeinterval)

  }

  assertthat::on_failure(test_22) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available timeinterval options. Please make sure the timeinterval argument is a character string of the following format: n unit (with n = number, and unit = one of 'sec', 'secs', 'min', 'mins', 'hour', 'hours', 'day', 'days', 'week', 'weeks', 'month', 'months', 'year', 'years'). Please consult the scales::breaks_width() documentation for more information.")

  }

  assertthat::assert_that(test_22(timeinterval))

  # 1.8. Check if the boolean flag arguments (smooth, interactive and save) are in the
  # correct format

  test_21 <- function(x){

    assertthat::is.flag(x)

  }

  assertthat::on_failure(test_21) <- function(call, env){

    paste0(deparse(call$x), " is not a Boolean flag (TRUE or FALSE). Please set argument argument to TRUE or FALSE. Make sure the argument is not a character string.")

  }

  assertthat::assert_that(test_21(smooth))
  assertthat::assert_that(test_21(interactive))
  assertthat::assert_that(test_21(save))

  # 1.9. Check if the supplied movavg argument abides by the expected format

  test_22 <- function(x){

    assertthat::is.count(x) &
      x > 0 &
      x < ncol(aggregated_soundscape@aggregated_df)
  }

  assertthat::on_failure(test_21) <- function(call, env){

    paste0(deparse(call$x), " is not a valid movavg argument. Please supply the movavg argument as a single positive integer with a value larger than zero and smaller than the number of unique times in the recording period.")

  }

  assertthat::assert_that(test_22(movavg))

  # 1.10. Check if the dir, filename and device arguments
  # follow the expected format.

  if (dir=="default"){
    dir <- getwd()
  }

  else{dir <- dir}

  test_35 <- function(x){

    assertthat::is.string(x)

  }

  test_36 <- function(x){

    assertthat::is.dir(x)

  }

  assertthat::on_failure(test_35) <- function(call, env){
    paste0(deparse(call$x), " is not a character string. The dir arguments needs to be a character string of either 'default' - or a valid pathname to an existing directory on your device. If you're working on a Windows operating system, pay attention to backslash and forwardslash.")

  }

  assertthat::assert_that(test_35(dir))
  assertthat::assert_that(test_36(dir))

  test_37 <- function(x){

    assertthat::is.string(x)

  }

  test_38 <- function(x){

    !(sub('.*\\.', '', filename) %in% c("eps", "ps","tex", "pdf", "jpeg",
                                        "tiff","png", "bmp","svg", "wmf"))

  }

  assertthat::on_failure(test_37) <- function(call, env){
    paste0(deparse(call$x), " is not a valid filename argument. The filename argument needs to be a character string.")
  }

  assertthat::on_failure(test_38) <- function(call, env){
    paste0(deparse(call$x), " is not a valid filename argument. Please make the filename argument you provide a character string without the extension.")
  }

  assertthat::assert_that(test_37(filename))
  assertthat::assert_that(test_38(filename))

  test_39 <- function(x){

    assertthat::is.string(x) &
      x %in% c("eps", "ps","tex", "pdf", "jpeg", "tiff",
               "png", "bmp","svg", "wmf")

  }

  assertthat::on_failure(test_39) <- function(call, env){
    paste0(deparse(call$x), " is not a valid device argument. The device argument needs to be a character string, and one of the following options: eps, ps, tex, pdf, jpeg, tiff, png, bmp, svg, wmf.")
  }

  assertthat::assert_that(test_39(device))

  # 1.11. Check that the output argument is in the correct format

  test_22 <- function(x){
    assertthat::is.string(x)
  }

  test_23 <- function(x){
    x %in% c("percentage", "raw")
  }

  assertthat::on_failure(test_22) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the output argument as a character string. Consult package documentation for available output argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_23) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available sounddiv output options. Please consult package documentation for available output argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_22(output))
  assertthat::assert_that(test_23(output))

  # 1.12. Check if the supplied height and width arguments
  # follow the expected format

  test_40 <- function(x){
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_40) <- function(call, env){
    paste0(deparse(call$x), " is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.")
  }

  assertthat::assert_that(test_40(height))
  assertthat::assert_that(test_40(width))


  # Create time-handling and subsetting objects

  tz <- aggregated_soundscape@tz

  if (minfreq=="default"){
    minfreq <- min(as.numeric(rownames(aggregated_soundscape@aggregated_df)))
  }

  else{minfreq <- minfreq}

  if (maxfreq=="default"){
    maxfreq <- max(as.numeric(rownames(aggregated_soundscape@aggregated_df)))
  }

  else{maxfreq  <- maxfreq}

  if (dir=="default"){
    dir <- getwd()
  }

  else{dir <- dir}

  # Create graphs

  if (graphtype=="total"){

    total_tod <- sounddiv(aggregated_soundscape = aggregated_soundscape,
                          qvalue = qvalue,
                          subset = "tod",
                          minfreq = minfreq,
                          maxfreq = maxfreq,
                          twilight = "sunlight",
                          freqseq = FALSE ,
                          nbins = nbins,
                          output = output)

    total_tod$soundscape_div_smooth <- pracma::movavg(total_tod$soundscape_div,
                                                      movavg,
                                                      type = "t")

    total_tod$time_of_day <- as.POSIXct(
      paste0(aggregated_soundscape@first_day,
             " ",
             total_tod$time_of_day),
      tz = aggregated_soundscape@tz)

    if (smooth==TRUE){

      plot <-
        ggplot2::ggplot(total_tod,
                        ggplot2::aes(time_of_day, soundscape_div_smooth)) +

        ggplot2::geom_area(alpha=0.25,
                           fill="#440154FF") +

        ggplot2::geom_line(color="#440154FF",
                           size=1) +

        ggplot2::ylab(if(output=="percentage"){"Soundscape diversity (%)\n"}
                      else{"Soundscape diversity (# OSUs)\n"}) +

        ggplot2::xlab("\nTime of day (h)") +

        ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                               tz = tz),
                                  breaks = scales::date_breaks(timeinterval),
                                  expand = c(0,0)) +

        ggplot2::scale_y_continuous(expand = c(0,0),
                                    limits = c(0,
                                               max(total_tod$soundscape_div_smooth)+10)) +

        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                "cm"),
                       panel.border = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black"),
                       axis.text.x = ggplot2::element_text(color = "black",
                                                           size = 10,
                                                           angle = -45,
                                                           vjust = 1.2,
                                                           hjust = -0.3),
                       axis.text.y = ggplot2::element_text(color = "black",
                                                           size = 10)) +

        ggplot2::annotate(geom = "text",
                          x = (max(total_tod$time_of_day)-10000),
                          y=if(output=="percentage"){95}
                          else{(max(total_tod$soundscape_div_smooth)+7.5)},
                          label = paste0("q-value = ",
                                         qvalue),
                          size = 5)

      if (interactive==TRUE){
        plotly::ggplotly(plot)}

      else{plot}

    }

    else{

      if (smooth==FALSE){

        plot <-

          ggplot2::ggplot(total_tod,
                          ggplot2::aes(time_of_day,
                                       soundscape_div)) +

          ggplot2::geom_area(alpha = 0.25,
                             fill="#440154FF") +

          ggplot2::geom_line(color = "#440154FF",
                             size=1) +

          ggplot2::ylab(if(output=="percentage"){"Total soundscape diversity (%)\n"}
                        else{"Total soundscape diversity (# OSUs)\n"}) +

          ggplot2::xlab("\nTime of day (h)") +

          ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                                 tz = tz),
                                    breaks = scales::date_breaks(timeinterval),
                                    expand = c(0,0)) +

          ggplot2::scale_y_continuous(expand = c(0,0),
                                      limits=c(0,
                                               max(total_tod$soundscape_div)+10)) +

          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.background = ggplot2::element_blank(),
                         plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                  "cm"),
                         panel.border = ggplot2::element_blank(),
                         axis.line = ggplot2::element_line(colour = "black"),
                         axis.text.x = ggplot2::element_text(color = "black",
                                                             size = 10,
                                                             angle = -45,
                                                             vjust = 1.2,
                                                             hjust = -0.3),
                         axis.text.y = ggplot2::element_text(color = "black",
                                                             size = 10)) +

          ggplot2::annotate(geom = "text",
                            x = (max(total_tod$time_of_day)-10000),
                            y=if(output=="percentage"){95}
                            else{(max(total_tod$soundscape_div)+7.5)},
                            label = paste0("q-value = ",
                                           qvalue),
                            size=5)

        if (interactive==TRUE){
          plotly::ggplotly(plot)
        }

        else{plot}

      }
    }
  }

  else{

    if (graphtype=="frequency"){

      freq_tod <- sounddiv(aggregated_soundscape = aggregated_soundscape,
                           qvalue = qvalue,
                           subset = "tod",
                           minfreq = minfreq,
                           maxfreq = maxfreq,
                           twilight = "sunlight",
                           freqseq = TRUE ,
                           nbins = nbins,
                           output = "raw")

      total_tod <- sounddiv(aggregated_soundscape = aggregated_soundscape,
                            qvalue = qvalue,
                            subset = "tod",
                            minfreq = minfreq,
                            maxfreq = maxfreq,
                            twilight = "sunlight",
                            freqseq = FALSE ,
                            nbins = nbins,
                            output = output)

      total_tod$soundscape_div_smooth <- pracma::movavg(total_tod$soundscape_div,
                                                        movavg,
                                                        type = "t")

      if (output == "percentage"){

        for (i in 1:length(freq_tod)){

          freq_tod[[i]][,1] <- (freq_tod[[i]][,1] /
                                  nrow(aggregated_soundscape@aggregated_df))*100

        }
      }

      for (i in 1:length(freq_tod)){
        freq_tod[[i]]$soundscape_div_smooth <- pracma::movavg(freq_tod[[i]][,1],
                                                              movavg,
                                                              type = "t")


        freq_tod[[i]]$frequency <- names(freq_tod)[i]
      }

      max_freq_raw <- max(total_tod$soundscape_div) + 15
      max_freq_smooth <- max(total_tod$soundscape_div_smooth) + 15

      freq_tod <- dplyr::bind_rows(freq_tod)

      freq_tod$time_of_day <- as.POSIXct(
        paste0(aggregated_soundscape@first_day,
               " ",
               freq_tod$time_of_day),
        tz = aggregated_soundscape@tz)

      freq_tod <- freq_tod[order(as.integer(sub("-.*$", "", freq_tod$frequency))),]
      freq_tod$frequency <- factor(freq_tod$frequency,
                                   levels = unique(freq_tod$frequency))

      if (smooth=="TRUE"){

        plot <-
          ggplot2::ggplot(freq_tod,
                          ggplot2::aes(time_of_day, soundscape_div_smooth,
                                       fill = frequency,
                                       color = frequency)) +

          ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE),
                             size = 0.5) +

          ggplot2::ylab(if(output=="percentage"){"Soundscape diversity (%)"}
                        else {"Soundscape diversity (#OSUs)"}) +

          ggplot2::xlab("Time of day (h)") +

          ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                                 tz = tz),
                                    breaks = scales::date_breaks(timeinterval),
                                    expand = c(0,0)) +

          ggplot2::scale_y_continuous(expand = c(0,0),
                                      limits=c(0,max_freq_smooth)) +

          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.background = ggplot2::element_blank(),
                         plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                  "cm"),
                         panel.border = ggplot2::element_blank(),
                         axis.line = ggplot2::element_line(colour = "black"),
                         axis.text.x = ggplot2::element_text(color = "black",
                                                             size = 10,
                                                             angle = -45,
                                                             vjust = 1.2,
                                                             hjust = -0.3),
                         axis.text.y = ggplot2::element_text(color = "black",
                                                             size = 10),
                         legend.justification=c(0.5,1),
                         legend.position=c(0.5, 1)) +

          viridis::scale_fill_viridis(
            alpha = 0.9,
            discrete = TRUE,
            guide = ggplot2::guide_legend(title = NULL,
                                          direction ="horizontal",
                                          nrow = 2,
                                          label.position = "top")) +

          viridis::scale_color_viridis(
            discrete = TRUE,
            guide = FALSE)

        if (interactive==TRUE){
          plotly::ggplotly(plot)}

        else{plot}

      }

      else{

        if (smooth=="FALSE"){

          plot <-
            ggplot2::ggplot(freq_tod, ggplot2::aes(time_of_day,
                                                   soundscape_div,
                                                   fill = frequency,
                                                   color = frequency)) +

            ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +

            ggplot2::ylab(if(output=="percentage"){"Soundscape diversity (%)"}
                          else {"Soundscape diversity (# OSUs)"}) +

            ggplot2::xlab("Time of day (h)") +

            ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                                   tz = tz),
                                      breaks = scales::date_breaks(timeinterval),
                                      expand = c(0,0)) +

            ggplot2::scale_y_continuous(expand = c(0,0),
                                        limits=c(0,max_freq_raw)) +

            ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                           panel.grid.minor = ggplot2::element_blank(),
                           panel.background = ggplot2::element_blank(),
                           plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                    "cm"),
                           panel.border = ggplot2::element_blank(),
                           axis.line = ggplot2::element_line(colour = "black"),
                           axis.text.x = ggplot2::element_text(color = "black",
                                                               size = 10,
                                                               angle = -45,
                                                               vjust = 1.2,
                                                               hjust = -0.3),
                           axis.text.y = ggplot2::element_text(color = "black",
                                                               size = 10),
                           legend.justification = c(0.5,1),
                           legend.position = c(0.5, 1)) +

            viridis::scale_fill_viridis(
              alpha = 0.9,
              discrete = TRUE,
              guide=ggplot2::guide_legend(title = NULL,
                                          direction = "horizontal",
                                          nrow = 2,
                                          label.position = "top")) +

            viridis::scale_color_viridis(
              discrete = TRUE,
              guide = FALSE)


          if (interactive==TRUE){
            plotly::ggplotly(plot)}

          else{plot}
        }
      }
    }

    else{

      if (graphtype=="normfreq"){

        freq_tod <- sounddiv(aggregated_soundscape = aggregated_soundscape,
                             qvalue = qvalue,
                             subset = "tod",
                             minfreq = minfreq,
                             maxfreq = maxfreq,
                             twilight = "sunlight",
                             freqseq = TRUE ,
                             nbins = nbins,
                             output = "raw")

        for (i in 1:length(freq_tod)){
          freq_tod[[i]]$soundscape_div_smooth <- pracma::movavg(freq_tod[[i]][,1],
                                                                movavg,
                                                                type="t")
          freq_tod[[i]]$frequency <- names(freq_tod)[i]
        }

        freq_tod=dplyr::bind_rows(freq_tod)

        freq_tod$time_of_day <- as.POSIXct(
          paste0(aggregated_soundscape@first_day,
                 " ",
                 freq_tod$time_of_day),
          tz = aggregated_soundscape@tz)

        freq_tod <- freq_tod[order(as.integer(sub("-.*$", "", freq_tod$frequency))),]
        freq_tod$frequency <- factor(freq_tod$frequency,
                                     levels = unique(freq_tod$frequency))


        if (smooth==TRUE){

          plot <-
            freq_tod %>%

            dplyr::group_by(time_of_day) %>%

            dplyr::mutate(prop_sound_div = soundscape_div_smooth /
                            sum(soundscape_div_smooth)) %>%

            dplyr::ungroup() %>%

            ggplot2::ggplot(ggplot2::aes(x = time_of_day,
                                         y = prop_sound_div,
                                         fill = frequency,
                                         color = frequency)) +

            ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +

            ggplot2::ylab("Contribution to total diversity") +

            ggplot2::xlab("Time of day (h)") +

            ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M",
                                                                 tz = tz),
                                      breaks = scales::date_breaks(timeinterval),
                                      expand = c(0,0)) +

            ggplot2::scale_y_continuous(expand = c(0,0),
                                        limits = c(0,1)) +

            ggplot2::theme(panel.grid.major = ggplot2::element_blank(),

                           panel.grid.minor = ggplot2::element_blank(),
                           panel.background = ggplot2::element_blank(),
                           plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                    "cm"),
                           panel.border = ggplot2::element_blank(),
                           axis.line = ggplot2::element_line(colour = "black"),
                           axis.text.x = ggplot2::element_text(color = "black",
                                                               size = 10,
                                                               angle = -45,
                                                               vjust = 1.2,
                                                               hjust = -0.3),
                           axis.text.y = ggplot2::element_text(color = "black",
                                                               size = 10),
                           legend.position = "top",
                           legend.key.width = grid::unit(3,
                                                         "cm"),
                           aspect.ratio = 0.3) +

            viridis::scale_fill_viridis(
              alpha = 0.9,
              discrete = TRUE,
              guide=ggplot2::guide_legend(title = NULL,
                                          direction = "horizontal",
                                          nrow = 2,
                                          label.position = "top")) +

            viridis::scale_color_viridis(
              discrete = TRUE,
              guide = FALSE)

          if (interactive==TRUE){
            plotly::ggplotly(plot)}

          else{plot}

        }

        else{

          if (smooth==FALSE){

            plot <-
              freq_tod %>%

              dplyr::group_by(time_of_day) %>%

              dplyr::mutate(prop_sound_div = soundscape_div / sum(soundscape_div)) %>%

              dplyr::ungroup() %>%

              ggplot2::ggplot(ggplot2::aes(x = time_of_day,
                                           y = prop_sound_div,
                                           fill = frequency,
                                           color = frequency)) +

              ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +

              ggplot2::ylab("Contribution to total diversity") +

              ggplot2::xlab("Time of day (h)") +

              ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                                     tz = tz),
                                        breaks = scales::date_breaks(timeinterval),
                                        expand = c(0,0)) +

              ggplot2::scale_y_continuous(expand = c(0,0),
                                          limits = c(0,1)) +

              ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                             panel.grid.minor = ggplot2::element_blank(),
                             panel.background = ggplot2::element_blank(),
                             plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                      "cm"),
                             panel.border = ggplot2::element_blank(),
                             axis.line = ggplot2::element_line(colour = "black"),
                             axis.text.x = ggplot2::element_text(color = "black",
                                                                 size = 10,
                                                                 angle = -45,
                                                                 vjust = 1.2,
                                                                 hjust = -0.3),
                             axis.text.y = ggplot2::element_text(color = "black",
                                                                 size = 10),
                             legend.position = "top",
                             legend.key.width = grid::unit(3,
                                                           "cm"),
                             aspect.ratio = 0.3) +

              viridis::scale_fill_viridis(
                alpha = 0.9,
                discrete = TRUE,
                guide=ggplot2::guide_legend(title = NULL,
                                            direction = "horizontal",
                                            nrow = 2,
                                            label.position = "top")) +

              viridis::scale_color_viridis(
                discrete = TRUE,
                guide = FALSE)

            if (interactive==TRUE){
              plotly::ggplotly(plot)}

            else{plot}

          }
        }
      }

      else{

        if (graphtype=="linefreq"){

          freq_tod <- sounddiv(aggregated_soundscape = aggregated_soundscape,
                               qvalue=qvalue,
                               subset = "tod",
                               minfreq=minfreq,
                               maxfreq=maxfreq,
                               twilight="sunlight",
                               freqseq=TRUE ,
                               nbins=nbins,
                               output=output)


          for (i in 1:length(freq_tod)){
            freq_tod[[i]]$soundscape_div_smooth <- pracma::movavg(freq_tod[[i]][,1],
                                                                  movavg,
                                                                  type="t")
            freq_tod[[i]]$frequency <- names(freq_tod)[i]
          }

          freq_tod=dplyr::bind_rows(freq_tod)

          freq_tod$time_of_day <- as.POSIXct(
            paste0(aggregated_soundscape@first_day,
                   " ",
                   freq_tod$time_of_day),
            tz = aggregated_soundscape@tz)

          freq_tod <- freq_tod[order(as.integer(sub("-.*$", "", freq_tod$frequency))),]
          freq_tod$frequency <- factor(freq_tod$frequency,
                                       levels = unique(freq_tod$frequency))


          if (smooth==TRUE){

            plot <-
              freq_tod %>%

              ggplot2::ggplot(ggplot2::aes(x = time_of_day,
                                           y = soundscape_div_smooth)) +

              ggplot2::geom_line(ggplot2::aes(color = frequency),
                                 size=1) +

              ggplot2::geom_area(ggplot2::aes(fill = frequency),
                                 alpha=0.2) +

              viridis::scale_color_viridis(discrete = TRUE) +

              viridis::scale_fill_viridis(discrete = TRUE) +

              ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                             panel.grid.minor = ggplot2::element_blank(),
                             panel.background = ggplot2::element_blank(),
                             plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                      "cm"),
                             panel.border = ggplot2::element_blank(),
                             axis.line = ggplot2::element_line(colour = "black"),
                             axis.text.x = ggplot2::element_text(color = "black",
                                                                 size = 10,
                                                                 angle = -45,
                                                                 vjust = 1.2,
                                                                 hjust = -0.3),
                             axis.text.y = ggplot2::element_text(color = "black",
                                                                 size = 10),
                             legend.position = "none",
                             strip.background = ggplot2::element_rect(color = "black",
                                                                      fill = "white"),
                             strip.text = ggplot2::element_text(color = "black",
                                                                size = 10)) +

              ggplot2::ylab(if(output=="percentage"){"Soundscape diversity (%)"}
                            else {"Soundscape diversity (# OSUs)"}) +

              ggplot2::xlab("Time of day (h)") +

              ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                                     tz = tz),
                                        breaks = scales::date_breaks(timeinterval),
                                        expand = c(0,0)) +

              ggplot2::scale_y_continuous(expand = c(0,0)) +

              ggplot2::facet_wrap(~frequency)

            if (interactive==TRUE){
              plotly::ggplotly(plot)}

            else{plot}
          }

          else{

            if (smooth==FALSE){

              labels <-
                paste0(
                  as.integer(
                    seq((minfreq-minfreq),
                        (maxfreq-(maxfreq/nbins)),
                        (maxfreq/nbins))),
                  "-",
                  as.integer(
                    seq((maxfreq/nbins),
                        maxfreq,
                        (maxfreq/nbins))),
                  " ",
                  "Hz")

              names(labels) <- seq((minfreq+(maxfreq/nbins)),
                                   maxfreq,
                                   (maxfreq/nbins))

              plot <-
                freq_tod %>%

                ggplot2::ggplot(ggplot2::aes(x = time_of_day,
                                             y = soundscape_div)) +

                ggplot2::geom_line(ggplot2::aes(color = frequency),
                                   size = 1) +

                ggplot2::geom_area(ggplot2::aes(fill = frequency),
                                   alpha=0.2) +

                viridis::scale_color_viridis(discrete = TRUE) +

                viridis::scale_fill_viridis(discrete = TRUE) +

                ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                               panel.grid.minor = ggplot2::element_blank(),
                               panel.background = ggplot2::element_blank(),
                               plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                        "cm"),
                               panel.border = ggplot2::element_blank(),
                               axis.line = ggplot2::element_line(colour = "black"),
                               axis.text.x = ggplot2::element_text(color = "black",
                                                                   size = 10,
                                                                   angle = -45,
                                                                   vjust = 1.2,
                                                                   hjust = -0.3),
                               axis.text.y = ggplot2::element_text(color = "black",
                                                                   size = 10),
                               legend.position = "none",
                               strip.background = ggplot2::element_rect(color = "black",
                                                                        fill = "white"),
                               strip.text = ggplot2::element_text(color = "black",
                                                                  size = 10)) +

                ggplot2::ylab(if(output=="percentage"){"Soundscape diversity (%)"}
                              else {"Soundscape diversity (# OSUs)"}) +

                ggplot2::xlab("Time of day (h)") +

                ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                                       tz = tz),
                                          breaks = scales::date_breaks(timeinterval),
                                          expand = c(0,0)) +

                ggplot2::scale_y_continuous(expand = c(0,0)) +

                ggplot2::facet_wrap(~frequency)

              if (interactive==TRUE){
                plotly::ggplotly(plot)}

              else{plot}

            }
          }
        }
      }
    }
  }

  if (save==TRUE){
    ggplot2::ggsave(filename = paste0(paste0(graphtype,
                                             "_"),
                                      filename,
                                      ".",
                                      device),
                    plot = plot,
                    device = device,
                    path = dir,
                    dpi = "retina",
                    width = width,
                    height = height,
                    units = c("mm"))

    if (interactive==TRUE){
      plotly::ggplotly(plot)}

    else{plot}
  }

  else{
    if (interactive==TRUE){
      plotly::ggplotly(plot)}

    else{plot}
  }
}


