# spflow (development version)

# spflow 0.0.0.9002

Added a test case to the previous version.
The case of model 2 without the intra-regional constant is now covered.
The package should allow to estimate all models under the symmetric flow case.

# spflow 0.0.0.9001

**This is a prerelease of the package only intended for beta testing.**
The functions that are ready for testing are listed below.
Estimation methods available at this point are s2sls; mle based on the mixed hessian calculation; and mcmc.

The only tested example corresponds to within network flows under sdm specification which includes the intra-regional parameters. 

+ data structures: `sp_network()` , `sp_network_pair()`, `sp_multi_network()`
+ estimation: `spflow()`
