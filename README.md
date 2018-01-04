# DineSafe Toronto Exploration  
Background: DineSafe is Toronto Public Health's food safety program that inspects all establishments serving and preparing food. Each inspection results in a pass, a conditional pass or a closed notice.

Objective: Can information about restaurants inspection be used to predict how long a restaurant stays in business? 

Data:
- Each establishment is inspected 1-3 times a year
- Inspection results for ~10,000 restaurants are available for the period of (2010-2017) (a total of 376,114 records)
- Other features include restaurant type, geographical location, and FSA code

Conclusions:
- Inspection results were used to generate a score (out of a 100) that summarizes overall inspection status per restaurant. A score closer to a 100 represents restaurants that follow the public health and safety guidelines, while a score closer to zero represent restaurants that fail to do so on regular basis
- Certain FSAs in Toronto are enriched with restaurants that have low inspection score
- Inspection score along with FSA and restaurant type information were used to build a classifier that can predict low/high risk restaurant classes
- There is a significant difference in restaurant survival time between the two classes, indicating the important role that inspection performance, along with location play in an establishment's longivity
