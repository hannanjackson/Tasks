setwd ('C:\\Users\\justj\\Desktop\\Evolution\\Tasks\\Task_11')

library("meme")
# Let's make a meme!
if (.Platform$OS.type == "windows") {
  windowsFonts(
    Arial = windowsFont("Arial"))
}
u <- system.file("monkey5.jpg", package="meme")
# I had some fun with this one!
meme("monkey5.jpg", upper="When you look in the mirror after a rough day at work", lower="Damn. The evolution.", font="Arial", color="white", size=1.5)
# Here's a more "serious" meme.
p <- system.file("crocodile2.jfif", package="meme")
meme("crocodile2.jfif", upper="Vertebrae: Gets to chill and eat all day", lower="Humans: Can I naturally select to do that?", vjust=0.1, font="Arial", bgcolor="red", color="white", size=1.5)