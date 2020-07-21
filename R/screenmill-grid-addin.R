#
# ************************************************************
#
#  Functions to modify grids that failed screenmill_calibrate
#
#  Almost all of these borrowed from Eric Edward Bryant
#
# ------------------------------------------------------------
#
#                   read_plate function
#
# reads screenmill-annotation and -calibration data for a specific plate_id
#
# Input
#   plate : a plate_id from screenmill annotations
#   dir : a directory containing screenmill data files
#   view : whether to display an image of the plate with grid overlay - if available
#
# Returns a lst of path, anno, crop, grid
#

#' read a single screenmill plate and annotation data by plate_id and display
#'
#' @param plate plate id from a screenmill annotation
#' @param dir directory containing screenmill data files
#' @param view logical display plate with grid defaults to TRUE
#' @export

read_plate <- function(plate = "2016-03-02-001-001-003", # or a previously read plate
                       dir = "data",
                       view = TRUE) {
  anno <- screenmill:::read_annotations(dir) %>% filter(.data$plate_id == !!plate)

  crop <-
    screenmill:::read_calibration_crop(dir) %>%
    semi_join(anno, by = c("template", "position"))

  grid <-
    read_csv(file.path(dir, 'screenmill-calibration-grid.csv'), col_types = cols()) %>%
    semi_join(anno, by = c("template", "position"))

  path <- file.path(dir, anno$file)

  img <- apply_calibration(path, anno, crop, grid)

  result <- lst(path, img, anno, crop, grid)
  if (view) view_plate(result)
  return(invisible(result))
}

apply_calibration <- function(path, anno, crop, grid) {
  img     <- screenmill:::read_greyscale(path, crop$invert)
  rough   <- with(crop, img[ rough_l:rough_r, rough_t:rough_b ])
  rotated <- EBImage::rotate(rough, crop$rotate)
  cropped <- with(crop, rotated[ fine_l:fine_r, fine_t:fine_b ])
  return(cropped)
}

update_plate <- function(plate, view = TRUE) {
  plate$img <- with(plate, apply_calibration(path, anno, crop, grid))
  if (view) view_plate(plate)
  return(invisible(plate))
}

#--------------------------------------------------------------------
#
#      view_plate
#
#' display single palte image with grid overlay
#'
#' @param plate a screenmill plate object loaded by the read_plate function
#' @param grid_color eponymous
#' @export
view_plate <- function(plate,grid_color='blue') {
  EBImage::display(plate$img, method = 'raster')
  with(plate$grid, segments(l, t, r, t, col = grid_color))
  with(plate$grid, segments(l, b, r, b, col = grid_color))
  with(plate$grid, segments(l, t, l, b, col = grid_color))
  with(plate$grid, segments(r, t, r, b, col = grid_color))
  return(invisible(plate))
}

#-------------------------------------------------------
#
#'   shifts plate grid as a unit
#'
#' @param plate a screenmill plate object loaded by the read_plate function
#' @param left number of pixels to move grid to the left
#' @param right number of pixels to move grid to the right
#' @param up number of pixels to move grid up
#' @param down number of pixels to move grid down
#' @param view logical display plate with grid defaults to TRUE
#' @export

shift_grid <- function(plate,
                       left = 0, right = 0, up = 0, down = 0,
                       view = TRUE) {
  plate$grid <-
    plate$grid %>%
    mutate(
      x = x + (right - left),
      y = y + (down - up),
      l = l + (right - left),
      r = r + (right - left),
      t = t + (down - up),
      b = b + (down - up)
    )
  if (view) view_plate(plate)
  return(invisible(plate))
}

#-------------------------------------------------------
#
#' shift_grid_cell
#'
#'   Adds a conditional to shift grid so that grid can be moved by colony_row,
#'    colony_col or both to specify a specific cell. The conditional should
#'    be captured with `quo` as such:
#'      \code{cond = quo(colony_row == 16 & colony_col%in%c(14:22))}
#'
#' @param plate a screenmill plate object loaded by the read_plate function
#' @param cond a quoted conditional statement to select range of rows or cols
#' @param left number of pixels to move grid to the left
#' @param right number of pixels to move grid to the right
#' @param up number of pixels to move grid up
#' @param down number of pixels to move grid down
#' @param view logical display plate with grid defaults to TRUE
#' @export
#
#
shift_grid_cell <- function(plate,cond,
                            left = 0, right = 0, up = 0, down = 0,
                            view = TRUE) {
  plate$grid <-
    plate$grid %>%
    mutate(
      x = if_else (!!cond,x + (right - left),x),
      y = if_else (!!cond,y + (down - up),y),
      l = if_else (!!cond,l + (right - left),l),
      r = if_else (!!cond,r + (right - left),r),
      t = if_else (!!cond,t + (down - up),t),
      b = if_else (!!cond,b + (down - up),b)
    )
  if (view) view_plate(plate)
  return(invisible(plate))
}


#
# ------------------------------------------------------------------------
#
#'              justify_grid_edge
#'
#'
#'
#' @param plate a screenmill plate object loaded by the read_plate function
#' @param cond a quoted conditional statement to select range of rows or cols
#' @param side one of "t", "b", "l", "r"
#' @param fcn function by which to align selected edge, median, max or min suggested
#' @param view logical display plate with grid defaults to TRUE
#' @export

justify_grid_edge <- function(plate,cond,side="t",fcn,view=TRUE) {

  target_value <- as.integer(fcn(pull(filter(plate$grid,!!cond)[,side])))
  # top or bottom
  if (side == "t" | side == "b") {
    if (side == "t") {
      plate$grid <-
        plate$grid %>%
        mutate(
          t = if_else (!!cond,target_value,t),
          y = if_else (!!cond,t + (b - t)%/%2,y),
        )
    } else { # else `b`
      plate$grid <-
        plate$grid %>%
        mutate(
          b = if_else (!!cond,target_value,b),
          y = if_else (!!cond,t + (b - t)%/%2,y),
        )
    }
 # left or right
  } else if (side == "l" | side == "r") {
    if (side == "l") {
      plate$grid <-
        plate$grid %>%
        mutate(
          l = if_else (!!cond,target_value,t),
          x = if_else (!!cond,t + (r - l)%/%2,x),
        )
    } else { # else `r`
        plate$grid <-
          plate$grid %>%
          mutate(
            r = if_else (!!cond,target_value,t),
            x = if_else (!!cond,t + (r - l)%/%2,x),
          )
      }
  }
  if (view) view_plate(plate)
  return(invisible(plate))
}

#
# ------------------------------------------------------------------------
#'
#'             standardize_grid
#'
#' Throwing some kind of bug
#'
#' @param plate plate id from a screenmill annotation
#' @param view logical display plate with grid defaults to TRUE
#' @export

standardize_grid <- function(plate,view=TRUE) {
  #
  # Note, these values are col_coubles in plate annotation data
  #
  plate$grid <-
    plate$grid %>%
    group_by(colony_row) %>%
    mutate(
      t = round(median(t),digits = 0),
      b = round(median(b),digits = 0),
      y = round(median(y),digits = 0)
    ) %>%
    ungroup() %>%
    group_by(colony_col) %>%
    mutate(
      l = round(median(l),digits = 0),
      r = round(median(r),digits = 0),
      x = round(median(x),digits = 0)
    ) %>%
    ungroup()
  if (view) view_plate(plate)
  return(invisible(plate))
}

#
# ------------------------------------------------------------------------
#
#'              align_grid_row
#'
#' @param plate a screenmill plate object loaded by the read_plate function
#' @param cond a quoted conditional statement to select range of rows or cols
#' @param side one of "t", "b"
#' @param view logical display plate with grid defaults to TRUE
#' @export

align_grid_row <- function(plate,cond,side="t", view=TRUE) {

  if (side == "t") {
    # compute highest cells in selected row
    a_top <- plate$grid %>%
      filter(!!cond) %>%
      pull(t) %>%
      min()
    #message (paste0("a_top = ",a_top))
    plate$grid <-
      plate$grid %>%
      mutate(
        y = if_else (!!cond,y + (a_top - t),y),
        t = if_else (!!cond,t + (a_top - t),t),
        b = if_else (!!cond,b + (a_top - t),b)
      )
  } else if (side == "b") {
    a_bot <- plate$grid %>%
      filter(!!cond) %>%
      pull(b) %>%
      max()
    message (paste0("a_bot = ",a_bot))
    plate$grid <-
      plate$grid %>%
      mutate(
        y = if_else (!!cond,y + (b - a_bot),y),
        t = if_else (!!cond,t + (b - a_bot),t),
        b = if_else (!!cond,b + (b - a_bot),b)
      )
  } else warning ('parameter side must be one of "t" or "b"')
  if (view) view_plate(plate)
  return(invisible(plate))
}

#------------------------------------------------------------------
#
#'    morph_grid_cell
#'
#'   Modifies grid by specific edges in order to shrink or expand as necessary
#'     to capture data or avoid plate anomalies (shadows/reflections/contaminants)
#'     Uses a conditional as in shift_grid_cell so that specific cells/rows/cols can be
#'     modified. x and y are recalculated when cells are updated. Positive values for
#'     left,right,up,down move named grid edges in that direction, negative values reverse this
#'
#' @param plate a screenmill plate object loaded by the read_plate function
#' @param cond a quoted conditional statement to select range of rows or cols
#' @param left number of pixels to move grid to the left
#' @param right number of pixels to move grid to the right
#' @param up number of pixels to move grid up
#' @param down number of pixels to move grid down
#' @param view logical display plate with grid defaults to TRUE
#' @export
#' @alias morph_grid_cell

sm_resize_grid_cell <- function(plate,cond,
                            left = 0, right = 0, up = 0, down = 0,
                            view = TRUE) {
  plate$grid <-
    plate$grid %>%
    mutate(
      x = if_else (!!cond,x + (right - left)%/%2,x),
      y = if_else (!!cond,y + (down - up)%/%2,y),
      l = if_else (!!cond,l - left,l),
      r = if_else (!!cond,r + right,r),
      t = if_else (!!cond,t - up,t),
      b = if_else (!!cond,b + down,b)
    )
  if (view) view_plate(plate)
  return(invisible(plate))
}
morph_grid_cell = sm_resize_grid_cell

#---------------------------------------------------------------------------
#
#' shift_crop
#'
#' shifts the crop of a plate by the specified pixel amount in the specified
#' direction
#'
#' @param plate a screenmill plate object loaded by the read_plate function
#' @param cond a quoted conditional statement to select range of rows or cols
#' @param left number of pixels to move grid to the left
#' @param right number of pixels to move grid to the right
#' @param up number of pixels to move grid up
#' @param down number of pixels to move grid down
#' @param view logical display plate with grid defaults to TRUE
#' @export

shift_crop <- function(plate,
                       left = 0, right = 0, up = 0, down = 0,
                       view = TRUE) {
  plate$crop <-
    plate$crop %>%
    mutate(
      fine_l = fine_l + (right - left),
      fine_r = fine_r + (right - left),
      fine_t = fine_t + (down - up),
      fine_b = fine_b + (down - up)
    )
  result <- update_plate(plate, view = view)
  return(invisible(result))
}

#---------------------------------------------------------------------------
#
#' sm_resize_crop
#'
#' shifts the crop of a plate by the specified pixel amount in the specified
#' direction
#'
#' @param plate a screenmill plate object loaded by the read_plate function
#' @param left number of pixels to move left grid-edge to the left, use negative values to move it to the right
#' @param right number of pixels to move right grid-edge to the right, use negative values to move it to the left
#' @param up number of pixels to move top grid-edge up, negative moves it down
#' @param down number of pixels to move bottom grid-edge down, negative moves it up
#' @param view logical display plate with grid defaults to TRUE
#' @export

sm_resize_crop <- function(plate,
                       left = 0, right = 0, up = 0, down = 0,
                       view = TRUE) {
  plate$crop <-
    plate$crop %>%
    mutate(
      fine_l = fine_l - left,
      fine_r = fine_r + right,
      fine_t = fine_t - up,
      fine_b = fine_b + down
    )
  result <- update_plate(plate, view = view)
  return(invisible(result))
}

#---------------------------------------------------------------------------
#
#' sm_rotate_crop
#'
#' rotates the crop of a plate by the specified angle. Default rotation from
#' standard scan template is 90˚. Providing a crop value of < 90˚ gives an
#' apparent counter-clockwise rotation to the plate. An angle of > 90˚ gives a
#' clockwise rotation.
#'
#' @param plate a screenmill plate object loaded by the read_plate function
#' @param rotate number of pixels to move grid to the left
#' @param view logical display plate with grid defaults to TRUE
#' @export

sm_rotate_crop <- function(plate,
                       new_rotate = 90,
                       view = TRUE) {
  plate$crop <-
    plate$crop %>%
    mutate(
      rotate = new_rotate
    )
  result <- update_plate(plate, view = view)
  return(invisible(result))
}


#-----------------------------------------------------------
#
#' save_plate_calibration
#'
#' saves modified annotation data to the appropriate screenmill
#' files after modifications
#'
#' @param plate A plate object constructed with the `read_plate` function
#' @export

save_plate_calibration <- function(plate) {
  new_anno <- plate$anno
  new_crop <- plate$crop
  new_grid <- plate$grid
  dir      <- dirname(plate$path)

  anno <-
    screenmill::read_annotations(dir) %>%
    anti_join(new_anno, by = "plate_id") %>%
    bind_rows(new_anno) %>%
    write_csv(file.path(dir, "screenmill-annotations.csv"))

  crop <-
    screenmill::read_calibration_crop(dir) %>%
    anti_join(new_crop, by = c("template", "position")) %>%
    bind_rows(new_crop) %>%
    write_csv(file.path(dir, "screenmill-calibration-crop.csv"))

  grid <-
    read_csv(file.path(dir, "screenmill-calibration-grid.csv"), col_types = cols()) %>%
    anti_join(new_grid, by = c("template", "position")) %>%
    bind_rows(new_grid) %>%
    write_csv(file.path(dir, "screenmill-calibration-grid.csv"))

  return(invisible(plate))
}

#
# ------------------------------------------------------------
#
#'                 use_calibration_from
#'
#' INPUT
#'    acceptor - a single screenmill plate image with associated metadata produced by read_plate
#'                'accepts' the donated grid
#'    donor    - a single screenmill plate image with associated metadata produced by read_plate
#'                'donates' a grid
#'
#'      progamatically, the acceptor gives the identity elements of the plate (image-file-path, group, etc).
#'      Identity data is copied onto the donor object with the desired crop and grid data
#'      The the 'donor' is updated, which reloads the image using the new image path etc.
#' Copies grid annotation data from the donor onto the acceptor image
#'
#' @param acceptor A screenmill plate image object produced by `read_plate`
#' @param donor A screenmill plate image object produced by `read_plate`
#' @export

use_calibration_from <- function(acceptor, donor) {
  donor$path          <- acceptor$path
  donor$anno          <- acceptor$anno
  donor$grid$template <- acceptor$anno$template
  donor$grid$position <- acceptor$anno$position
  donor$grid$group    <- acceptor$anno$group
  donor$crop$template <- acceptor$anno$template
  donor$crop$position <- acceptor$anno$position
  result <- update_plate(donor)
  return(invisible(result))
}

#---------------------------------------------------------------------------
#
#' sm_regrid
#'
#' Input: a plate object from the `read_plate` function which contains
#' annotation, crop and grid data for a specific plate image.
#' Grid data is replaced by a call to `screenmill:::locate_grid`.
#' Used to either redo a grid or make a de novo grid after fixing crop issues
#'
#' @param plate_obj a screenmill plate object loaded by the read_plate function
#' @param grid_rows number of colony rows on plate image
#' @param grid_cols number of colony columns on plate image
#' @param replicates number of replicates in each plate grid. This is always a square; e.g.
#' 1,4,16
#' @param colony_radius defaults to 1
#' @param max_smooth defaults to 5
#' @export

sm_regrid <- function(plate_obj,grid_rows,grid_cols,replicates,colony_radius=1,max_smooth=5,view=TRUE) {
  p                <- plate_obj$anno$position
  collection_id    <- plate_obj$anno$strain_collection_id
  collection_plate <- plate_obj$anno$plate
  group            <- plate_obj$anno$group
  #finei            <- fine[which(fine$position == p), ] # row of fine crop info - don't care, already cropped
  #keyi  <- with(key, key[which(strain_collection_id == collection_id & plate == collection_plate), ]) # rows of key info for plate
  #plate <- plate_obj$img

  #if (invert) plate <- 1 - plate
  #rotated <- EBImage::rotate(plate, finei$rotate)
  #cropped <- with(finei, rotated[fine_l:fine_r, fine_t:fine_b])

  result <- screenmill:::locate_grid(plate_obj$img, grid_rows, grid_cols, radius = colony_radius, max_smooth = max_smooth)

  if (is.null(result)) {
    warning(
      'Failed to locate colony grid for ', plate_obj$anno$plate_id,
      ' at position ', p, '. This plate position has been skipped.\n',
      call. = FALSE)
  } else {
    # Annotate result with template, position, strain collection and plate
    result <-
      mutate(result,
             template             = plate_obj$anno$template,
             position             = plate_obj$anno$position,
             group                = plate_obj$anno$group,
             strain_collection_id = plate_obj$anno$strain_collection_id,
             plate                = plate_obj$anno$plate
             )

    # Check the grid size and compare to expected plate size
    #replicates <- nrow(result) / nrow(keyi)

    # if (sqrt(replicates) %% 1 != 0) {
    #   result <- NULL
    #   warning(
    #     'Size of detected colony grid (', nrow(result), ') for ',
    #     basename(template), ' at position ', p,
    #     ' is not a square multiple of the number of annotated positions (',
    #     nrow(keyi), ') present in the key for ', collection_id,
    #     ' plate #', collection_plate, '. This plate position has been skipped.\n', call. = FALSE
    #   )
    #    } else {
    # Annotate with key row/column/replicate values
    sqrt_rep <- sqrt(replicates)
    key_rows   <- 1:(grid_rows/sqrt_rep)
    key_cols   <- 1:(grid_cols/sqrt_rep)
    n_rows   <- length(key_rows)
    n_cols   <- length(key_cols)
    one_mat  <- matrix(rep(1, times = n_rows*n_cols), nrow = n_rows, ncol = n_cols)

    rep_df <-
      (one_mat %x% matrix(1:replicates, byrow = T, ncol = sqrt_rep)) %>%
      as.data.frame %>%
      tibble::rownames_to_column('colony_row') %>%
      gather('colony_col', 'replicate', starts_with('V')) %>%
      mutate(
        colony_row = as.integer(colony_row),
        colony_col = as.integer(gsub('V', '', colony_col)),
        replicate  = as.integer(replicate)
      )

    col_df <-
      matrix(rep(key_cols, each = n_rows * replicates), ncol = n_cols * sqrt_rep) %>%
      as.data.frame %>%
      tibble::rownames_to_column('colony_row') %>%
      gather('colony_col', 'column', starts_with('V')) %>%
      mutate(
        colony_row = as.integer(colony_row),
        colony_col = as.integer(gsub('V', '', colony_col))
      )

    row_df <-
      matrix(rep(key_rows, each = n_cols * replicates), nrow = n_rows * sqrt_rep, byrow = T) %>%
      as.data.frame %>%
      tibble::rownames_to_column('colony_row') %>%
      gather('colony_col', 'row', starts_with('V')) %>%
      mutate(
        colony_row = as.integer(colony_row),
        colony_col = as.integer(gsub('V', '', colony_col))
      )

    result <-
      result %>%
      left_join(row_df, by = c('colony_row', 'colony_col')) %>%
      left_join(col_df, by = c('colony_row', 'colony_col')) %>%
      left_join(rep_df, by = c('colony_row', 'colony_col')) %>%
      select(template:replicate, colony_row:b, everything())
    #    }
  }

  if (view) view_plate(result)
  return(invisible(result))
}
