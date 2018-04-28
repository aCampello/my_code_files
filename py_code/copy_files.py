# -*- coding: utf-8 -*-
"""
2018-04-08

Copy files from CameraBase to Desktop for use in the CV tail position project.

1. Copy photos of yes fox and no fox.
2. Copy photos of >1 individual fox.


"""
import pandas as pd
import os
import shutil
from tqdm import tqdm
import time
##############################################################################


##### 1. Copy photos of yes fox and no fox.



# for fox/no fox just need a two key dict with survey ID (to specify search directory)
# and image name, i.e. for surveys 1 and 2
eg_dict = {[1]:[image1.jpg, image2.jpg, image3.jpg],
           [2]: [image4.jpg, image5.jpg]}


## for fox tail positions need a nested dict with survey ID as the first level key and
# tailpos as the second, i.e. for surveys 1 and  with tail positions A and B
eg_dict = {
        [1]:
            {[A]: [image1.jpg, image2.jpg, image3.jpg],
             [B]: [image4.jpg, image5.jpg]},
        [2]: {[A]: [image6.jpg, imag7.jpg],
              [B]: [image8.jpg, image9.jpg]}
        }

######
# TEST - to group by multiple columns:
#a = no_fox.groupby(['SurveyID', 'CaptureID'])['Image1'].apply(lambda x: x.tolist())
#a.head()
######

########################################################################

# load data
no_fox = pd.read_excel('C:/Users/User/Desktop/fox_tail_position_project/image_lists_from_camerabase/CV_img_no_fox_random_order_20180415.xlsx')
yes_fox = pd.read_excel('C:/Users/User/Desktop/fox_tail_position_project/image_lists_from_camerabase/CV_img_with_fox_random_order_limit65k_20180415.xlsx')

# take the first 5000 rows of each (as they are ordered by random)
no_fox_5000 = no_fox.head(5000)
yes_fox_5000 = yes_fox.head(5000)

# make dict with list of images per survey
images_no_fox = no_fox_5000.groupby('SurveyID')['Image1'].apply(lambda x: x.tolist()).to_dict()

images_yes_fox = yes_fox_5000.groupby('SurveyID')['Image1'].apply(lambda x: x.tolist()).to_dict()


def copy_files(to_copy, source_dir, destination_dir):
    """
    Copy files in the to_copy list from the source directory to the destination directory.
    Helper function called by make_copies().
    """
    log_file = []
    files = os.listdir(source_dir)
    num_files_to_copy = len(to_copy)
    progress_counter = 1
    for file in files:
        if file in to_copy:
            # shutil.copy2() preserves the metadata
            shutil.copy2(src = source_dir + "/" + file, dst = destination_dir)
            message = "File " + str(progress_counter) + "/" + str(num_files_to_copy) + ": " + file + "copied from " + source_dir + " to " + destination_dir
            #print(message)
            log_file.append(message)
            progress_counter += 1
    return log_file


def make_copies(img_dict, main_img_dir, copies_img_dir):
    """
    Copy images from hard drive to folder for CV project.
    First locates the source and destination directories and uses these as input to the copy_files function.
    img_dict: dict with keys as survey IDs and values as list of image names.
    """

    for survey, images in tqdm(img_dict.items()):
        t0 = time.time()
        n_images = len(images)
        print("Copying ", n_images, " files for survey ", survey, "...")
        source_dir = main_img_dir + str(survey)
        destination_dir = copies_img_dir + str(survey)

        # make directory labelled with survey ID if one doesnt already exist
        if not os.path.exists(destination_dir):
            os.makedirs(destination_dir)

        log_file = copy_files(to_copy = images, source_dir = source_dir, destination_dir = destination_dir)

        print("Finished copying files for survey", survey)
        t1 = time.time()
        print("Time taken: ", t1-t0, "s")
        print()

    return log_file



# set the paths
main_img_dir = 'G:/Bristol - PhD Camera Trapping Study/CameraBaseFoxStudy2013-15/Images/'
yes_fox_img_dir = 'C:/Users/User/Desktop/fox_tail_position_project/images_yes_fox_no_fox/yes_fox_images/'
no_fox_img_dir = 'C:/Users/User/Desktop/fox_tail_position_project/images_yes_fox_no_fox/no_fox_images/'

# note the log files will only include the last survey as they're made in the
# make_copies query, which loops within each survey!

log_file_no_fox = make_copies(img_dict = images_no_fox,
                              main_img_dir = main_img_dir,
                              copies_img_dir = no_fox_img_dir)

log_file_yes_fox = make_copies(img_dict = images_yes_fox,
                              main_img_dir = main_img_dir,
                              copies_img_dir = yes_fox_img_dir)

###############################################################################

##### 2. Copy photos of >1 individual fox.


# load data
multi_fox = pd.read_excel('C:/Users/User/Desktop/fox_tail_position_project/image_lists_from_camerabase/CV_img_multiple_foxes_only_random_order_possible_duplicates_20180423.xlsx')

# drop duplicate images
multi_fox = multi_fox.drop(['randomID', 'CaptureID'], axis = 1)
multi_fox = multi_fox.drop_duplicates(subset=None, keep='first', inplace=False)

# take random sample
multi_fox = multi_fox.sample(n=5000)
len(multi_fox)

# write this sample to csv to send to Antonio
multi_fox.to_csv('C:/Users/User/Desktop/fox_tail_position_project/image_lists_from_camerabase/multiple_fox_photo_list_5000randomimgs.csv')

# make dict with list of images per survey
multi_fox_dict = multi_fox.groupby('SurveyID')['Image1'].apply(lambda x: x.tolist()).to_dict()

# set the paths
main_img_dir = 'G:/Bristol - PhD Camera Trapping Study/CameraBaseFoxStudy2013-15/Images/'
multi_fox_img_dir = 'C:/Users/User/Desktop/fox_tail_position_project/images_yes_fox_no_fox/multi_fox_images/'

log_file_multi_fox = make_copies(img_dict = multi_fox_dict,
                              main_img_dir = main_img_dir,
                              copies_img_dir = multi_fox_img_dir)


### compress folders in a directory
import os, shutil

cd C:/Users/User/Desktop/fox_tail_position_project/images_yes_fox_no_fox/multi_fox_images/

for directory in os.listdir(multi_fox_img_dir):
    shutil.make_archive(directory, 'zip', multi_fox_img_dir + directory)