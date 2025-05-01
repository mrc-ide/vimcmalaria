### vimcmalaria package

This package contains helper functions for [VIMC_malaria](https://github.com/mrc-ide/VIMC_malaria/tree/main), a package used to estimate malaria vaccine impact for the Vaccine Impact Modelling Consortium (VIMC). A roadmap of helper functions is found below:


```
.
├── R
|   ├── site_file                  
|   |   ├── remove_zero_eirs
|   |   ├── check_eir
|   ├── workflow                  
|   |   ├── completed_reports
|   |   ├── make_parameter_map
|   |   ├── check_reports_completed
|   |   ├── check_not_a_rerun
|   |   ├── run_local_reports
|   |   ├── submit_by_core
|   |   ├── pull_postprocessed_output
|   |   ├── get_site_output
|   |   ├── get_dose_output
|   |   ├── get_raw_output
|   |   ├── make_parameter_map
|   |   ├── compile_diagnostics
|   |   ├── compile_final_outputs
|   |   ├── compile_and_save
|   |   ├── compile_dose_output
|   |   ├── pull_dose_output
|   ├── analyse                  
|   |   ├── analyse_site
|   |   ├── make_analysis_map                     
|   ├── parameterise                     # Functions used to parameterize model
|   |   ├── pull_input_params            # Summary parameterization function
|   |   ├── pull_age_groups_time_horizon            
|   |   ├── parameterize_stochastic_run            
|   |   ├── update_coverage_values            
|   |   ├── expand_intervention_coverage            
|   |   ├── pull_input_params            
|   ├── model                  
|   |   ├── run_model                  
|   ├── postprocess                  
|   |   ├── process_output
|   |   ├── expand_life_expectancy
|   |   ├── vimc_postprocess
|   |   ├── format_outputs
|   |   ├── scale_population
|   |   ├── pull_doses_output
|   |   ├── reformat_output
|   |   ├── pull_low_transmission_sites
|   |   ├── append_low_transmission_sites
|   |   ├── scale_par
|   |   ├── process_output
|   |   ├── add_proportions
|   |   ├── scale_cases_deaths
|   |   ├── site_postprocessing               
└── README.md                            # Project overview


```
