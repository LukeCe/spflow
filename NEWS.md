# spflow 0.0.0.9006

**This is a prerelease of the package only intended for beta testing.**

Add ols estimation for model 1 to complete the case of symmetric flows which are a cartesian product of origins and destinations.

# spflow 0.0.0.9005

**This is a prerelease of the package only intended for beta testing.**

Add Bivands fitted values (TS) as it is fast to compute.
Add a vignette for the main functionality (which is still work in progress).
Create tests for all models and estimators in the symmetric case.
Model 2 to model 9 are run for the selected test case but the non-spatial model needs to be developed for all estimator.


# spflow 0.0.0.9004

**This is a prerelease of the package only intended for beta testing.**

Added predict method which can handle in sample prediction (fitted values).
Model results have their own class [spflow_model_meta()]:
+ `predict()` -method can handle in sample prediction (fitted values)
+ includes a goodness-of-fit measure
+ corrected t-test degrees of freedom 
+ includes example data for commuting flows of Paris

# spflow 0.0.0.9003

**This is a prerelease of the package only intended for beta testing.**
Included a first version of documentation for all functions classes and methods.

# spflow 0.0.0.9002

**This is a prerelease of the package only intended for beta testing.**
Added a test case to the previous version.
The case of model 2 without the intra-regional constant is now covered.
The package should allow to estimate all models under the symmetric flow case.

# spflow 0.0.0.9001

**This is a prerelease of the package only intended for beta testing.**
The functions that are ready for testing are listed below.
Estimation methods available at this point are s2sls; mle based on the mixed hessian calculation; and mcmc.

The only tested example corresponds to within network flows under sdm specification which includes the intra-regional parameters. 

+ data structures: `sp_network_nodes()` , `sp_network_pair()`, `sp_multi_network()`
+ estimation: `spflow()`
