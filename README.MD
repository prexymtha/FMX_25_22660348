---
output:
  md_document:
    variant: markdown_github
---

# Description

This folder was created by first creating a folder on your desktop - copying the adress and then simply running in any instance of R:


```{r, eval = F}
fmxdat::make_project()
```


Next, download https://www.fmx.nfkatzke.com/FMX_data_2021.zip and unzip its contents into the data/ folder in this root.

> NB: add data/ to your gitignore file when creating a repository. Ensure you do not commit the data folder please.

Next, create sub-folders for each question by using, e.g.:

```{r}

# PDF Texevier:
Texevier::create_template(directory = "Questions",
                          template_name = "Question1"
)

# HTML Texevier:
Texevier::create_template_html(directory = "Questions",
                          template_name = "Question2"
)

```

Now add the relevant data for each question into the respective data/ folders in the different Question folders.

Check that your .gitignore file ignores all folders starting with data.


> NB - this README (at the root of your folder) - is what I will be grading. 

> Space out code per question in this README - detailing your steps and thinking.

> Folders in Questions/... should not contain READMEs - I will only be looking at their output (pdf / html).

> THIS README should contain and describe the code you used in your analyses.


Good luck and enjoy the practical!

Nico

