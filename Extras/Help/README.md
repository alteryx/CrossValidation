# About the CrossValidation Tool

<img src="../../CrossValidationIcon.png" width=48 height=48 />



The Cross-Validation tool compares the performance of one or more Alteryx-generated predictive models using the process of [cross-validation](https://en.wikipedia.org/wiki/Cross-validation_(statistics)). Among predictive modelers, cross-validation is frequently preferred over other model evaluation methods because it does not require the use of a separate test set, and it generates more robust estimates of model quality. This tool supports all classification and regression models.

For all classification models, the tool provides the overall accuracy, the accuracy by class, and a set of confusion matrices (one for each model). Additionally, the tool reports the F1 score and a collection of Performance Diagnostic Plots (lift curve, gain chart, precision vs recall curves and ROC curve) for binary classification models. For regression models, the tool generally provides the correlation between predicted and actual values, the [root mean square error](https://en.wikipedia.org/wiki/Root-mean-square_deviation) (RMSE), the [mean absolute error](https://en.wikipedia.org/wiki/Mean_absolute_error) (MAE), the [mean percentage error](https://en.wikipedia.org/wiki/Mean_percentage_error) (MPE), and the [mean absolute percentage error](https://en.wikipedia.org/wiki/Mean_absolute_percentage_error) (MAPE) of each model's predictions. But when at least one target value is near 0, the MPE and the MAPE are undefined. So in this case, the MPE is replaced with the sum of the errors over the sum of the actual values, and the sum of the absolute errors divided by the sum of the actual values (ie the Weighted Absolute Percentage Error) replaces the MAPE. Additionally, the tool always provides a plot of actual vs predicted values in the regression case.

This tool is not automatically installed with Alteryx Designer. To use this tool, download it from the [Alteryx Analytics Gallery](https://gallery.alteryx.com/).


_Note: This tool uses the R tool. Install R and the necessary packages by going to Options > Download Predictive Tools._

### Inputs

The Cross-Validation tool requires 2 inputs.

1.  **D input**: The dataset used to generate the above models.
2.  **M input:** Either a single Alteryx-generated predicted model, or the union of two or more such models. These models should all have been generated using the same dataset.

### Configuration

1. __Number of trials:__ Enter the number of times you would like the cross-validation procedure to be repeated. Choosing a smaller number of trials will speed up the tool, but a larger number will give you a more robust estimate of your models' quality.
2. __Number of folds:__ Enter the number of subsets you would like the data split into. An analagous tradeoff to the one for the number of trials also exists for the number of folds.
3. __Should stratified cross-validation be used?:__ Stratified cross-validation is a special type of cross-validation that creates folds with the same probability distribution as the larger dataset. For example, in a dataset where 80% of the target values are “No,” and 20% are “Yes,” each fold would have roughly 80% “No” responses and 20% “Yes” ones. Stratified cross-validation is frequently recommended when the target variable is imbalanced.
4. __Name of the positive class (Optional, only relevant for binary classification):__ This configuration option is only relevant in binary (two-class) classification. Some of the measures reported for binary classification, such as the F1 score, require a distinction between a positive class (such as “Yes”) and a negative class (such as “no”). However, this configuration option is not required. If you leave it blank when using the tool with binary classification models, the tool will choose one of the classes as the positive one.
5. __Should graphs of the fit measures be displayed? (Not recommended if the product of the number of trials and the number of folds is large.):__ If you wish to view the graphs of the fit measures described in the introduction, you should check this option. If not, you should leave it unchecked, because not creating these graphs will speed up the tool.

### Output

1. **D output**: This output provides the actual data values as well as their predictions.
2. **F output:** This output reports various model fit measures, depending on model type (see the second paragraph for more details on these measures).
