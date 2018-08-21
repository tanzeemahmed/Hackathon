# Hackathon
Efficient project management by Employee Over-Time prediction

A manufacturing unit is consistently facing project over-time to finish. This is usually a sign
for poor project planning or resource allocation. Now the company has decided smartly
allocate the resource based on ML models. One indicator they have about project going
out-of-budget is number of employees working overtime. If a good predictive model can
help them predict which employees are expected to do overtime, then they can balance
the resource allocation accordingly.
Your job is to build a Machine Learning model which will predict whether an employee will
bill overtime on a project or not.
Evaluation Metric: If Accuracy>65 then give the score for Recall
 Datasets:
You have been provided MiTHDataset.csv which contains both train and test samples. A
sample can be treated as train sample If istrain=1 otherwise it is treated as test sample.
Train samples has Target value , whereas test samples does not have Target value. It was
kept as NA.
You need to predict Target for the test samples and upload your predictions in the
samplesubmission.csv format.


Attributes:
S.NO Attribute 
1 Age 
2 Frequencyof_Travel 
3 DailyRate 
4 Division 
5 DistancetoOffice 
6 Education 
7 Specialization 
8 No.of Employees 
9 EmployeeID 
10 OfficeAmbianceRating 
11 Gender 
12 HourlyRate 
13 SelfMotivated 
14 JobLevel 
15 Designation 
16 Happynesslevel in job 
17 MaritalStatus 
18 SalaryperMonth 
19 MonthlyRate
20 NumberofCompaniesChanged
21 Over18
22 Working Extratime(Target)
23 PercentSalaryHike
24 PerformanceRating
25 RelationshipSatisfaction
26 StandardHours
27 ESOPs(stackoptions)
28 DateOnwhich_datacollected
29 NoofTrainings_Attended
30 WorkLifeBalance
31 DateOfjoininginthe_CurrentCompany
32 No of years with current designation
33 YearsSinceLastPromotion
34 YearsWithCurrManager
35 Istrain- (1 train sample,0 test sample)
36 NoofTrainings_Attended
37 WorkLifeBalance
