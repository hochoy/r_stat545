---
output: html_document

---
# Homework 1
## by Wai Ho Choy
##### UBC Experimental Medicine MSc. student studying probiotics ~~and chicken soup~~
___
### Today's chicken soup recipe
1. *Delicious*
2. Tasty
3. Nutritious
    * Protein 50%
    * Love 30%
    * ~~Fat! 20%~~
    * **Now low-fat for only 9.99!**

![alt text](http://40.media.tumblr.com/1fefc582748094dbed979b8126577a42/tumblr_mjorqizkmj1r47bczo1_1280.jpg "Image is way too large, aparrently I can't do anything with markdown alone.jpg") 

```python
ingredient_1 = "Chicken (2lbs.)"
ingredient_2 = "Leeks (1 stick)"
ingredient_3 = "Water (1 pot)"
ingredient_4 = "Mother's love (1 teaspoon)"
print (ingredient_1,ingredient_2,ingredient_3,ingredient_4)
```

### How I made and git-ted this:

> First, I looked up 'Markdown syntax' and found this [Markdown Cheatsheet link](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) but I was unable to get git version control to run on just .md and .html in RStudio without an RStudio project so I did all my git-ting on the cmdline.

### On a mac osx:
```
cd desired_parent_folder
git clone https://github.com/STAT545-UBC/wai-ho_choy.git
```
> Edited the README.md file
```
cd wai-ho_choy
git status
git add .
git commit -m "message that describes the commit"
git remote -v              <- to check if I have the correct destination
git push origin master     <- push the changes from this local folder to the remote repository

```

### Proudly sponsored by: 
<iframe src="https://www.unrealengine.com/html5/" name="TappyChicken" 
   width="800" height="600" frameborder="0" scrolling="no">
   <p>Your browser does not support iframes.</p> >
</iframe>
> Code above is html. I guess the markdown -> html conversion works even if its already html


