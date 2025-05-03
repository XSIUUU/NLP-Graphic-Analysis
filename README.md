# NLP-Graphic-Analysis (STUDY OF IG INFLUENCER)


# 📊 Assignment 2: Multimodal Social Media Engagement Analysis

This project analyzes how **textual and image-based features** of Instagram posts impact user engagement (likes & comments) using advanced NLP and computer vision techniques.

---

## 🗂️ Project Structure

```
.
├── dmi_assignment 2_new.R              # Main R script with full pipeline
├── DMI_Course_Codes_final.ipynb        # Supplementary Python code (image scraping, pre-processing)
├── Presentation Group 2.1.pdf          # Final presentation slides
├── scrapping_dmi.xlsx                  # Instagram influencer post data
├── detr_object_detection_results.xlsx  # Object detection output
├── color_analysis_results.xlsx         # Image color metrics
```

---

## 🔍 Key Steps & Features

1. **Data Preparation & Cleaning**
   - Merged influencer, image, and object data
   - Standardized and transformed features (e.g., likes, dates, captions)

2. **NLP-Based Text Feature Extraction**
   - Word count, emoji count, punctuation analysis
   - Most frequent words, word cloud, sentiment scoring using:
     - AFINN
     - BING
     - NRC
     - VADER

3. **Image Feature Integration**
   - Brightness, contrast, saturation, warmth
   - Object count (total & unique) from DETR model

4. **Text & Image Modeling**
   - Lexicon-based and structural feature creation
   - LDA Topic Modeling (auto-selected optimal topics, top 20 terms/topic)

5. **Regression Modeling**
   - Random Forest & Stepwise Linear Models for likes/comments
   - Feature selection with quadratic & interaction terms
   - Visualizations with `interactions` package

---

## 🧠 Output & Presentation

- Results were interpreted and visualized to help explain key patterns to teammates
- Delivered findings in class via a PowerPoint presentation
- Regression plots revealed non-linear and interaction effects between visual/textual cues and user engagement

---

## 🛠️ Requirements

- R packages: `dplyr`, `ggplot2`, `tidytext`, `tm`, `quanteda`, `wordcloud2`, `topicmodels`, `randomForest`, `interactions`, `MASS`, `car`
- Python (optional): `OpenCV`, `transformers`, etc. (used in `ipynb`)

---

## 📎 Authors

Assignment completed as part of the **Digital Marketing Intelligence** course – MSc MADS, University of Groningen.

