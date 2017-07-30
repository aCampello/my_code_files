#!/usr/bin/python

from nltk.stem.snowball import SnowballStemmer
import string

def parseOutText(f):
    """ given an opened email file f, parse out all text below the
        metadata block at the top
        (in Part 2, you will also add stemming capabilities)
        and return a string that contains all the words
        in the email (space-separated) 
        
        example use case:
        f = open("email_file_name.txt", "r")
        text = parseOutText(f)
    """
    f.seek(0)  ### go back to beginning of file (annoying)
    all_text = f.read()

    ### split off metadata
    content = all_text.split("X-FileName:")
    words = ""
    if len(content) > 1:
        ### remove punctuation
        text_string = content[1].translate(string.maketrans("", ""), string.punctuation)

        ### project part 2: comment out the line below
        # words = text_string  # commented this line out for miniproject part 2
        
        ### Stem the words
        from nltk.stem.snowball import SnowballStemmer
        stemmer = SnowballStemmer("english")
        # split the text string into individual words
        for word in text_string.split():
            # append the stemmed word to words (separated by a single space)
            words = words + stemmer.stem(word) + " "  # add a space after each word
    return words


# Print out the text using parseOutText
def main():
    ff = open("../text_learning/test_email.txt", "r")
    text = parseOutText(ff)
    print text

if __name__ == '__main__':
    main()

