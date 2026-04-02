library(rDataRecon)
library(nycflights13)

#base df
planes_b <- planes

#compare df
planes_c <- planes
planes_c$engines_type <- paste(planes_b$engines,planes_b$engine,sep = "-") #to get Section 1
planes_c$engines <- as.character(planes_c$engines) # to get Section 2
planes_c$year[1] <- 2026
planes_c$manufacturer[2] <-"ganesh"
##for base_var and compare_var testing
planes_c$speed[2] <- 34
planes_c$speed1<- NA
planes_c$speed1[2]<- 2

result <- recon(planes_b, planes_c,
                id = "tailnum",
                #var =c("tailnum" ,"engines","year","manufacturer","speed","speed1") , #includes only the variable for comparison
                #base_var = "speed",
                #compare_var = "speed1",
                noequal=FALSE, # SUPRESS Variables with no differences in SECTION 3(Variables with no differences )
                listall = FALSE,#list all variable names in each dataset in the header section of the report. in SECTION 1
                brief =FALSE, #print only the summary section and suppress the row-level difference table in SECTION 3
                out =NULL, #the complete report is written to that file in addition to being printed only .txt
                out_data=TRUE)

#rDataRecon_result -A named list of class "rDataRecon_result" (returned invisibly) with elements:
result$summary
print(result$summary)
print(result$vars_base_only)
print(result$vars_compare_only)
print(result$type_mismatches)
print(result$value_diffs)
print(result$verdict)

#helper functions
summary(result)
print(result)
compare_stats(planes_b, planes_c)
is_structure_equal(planes_b,planes_c)
is_structure_equal(planes,planes_b)
get_diffs(result)


base2 <- data.frame(id = 1:3, revenue = c(100, 200, 300))
comp2 <- data.frame(id = 1:3, sales   = c(100, 250, 300))
recon(base2, comp2, id = "id",
      base_var = "revenue", compare_var = "sales")
