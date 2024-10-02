#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Sep 22 11:56:12 2024

@author: alexfion
"""

import pandas as pd
import numpy as np
from collections import Counter
from openai import OpenAI
import os
from sklearn.preprocessing import MultiLabelBinarizer

protest_1 = pd.read_csv('ccc_compiled_2017-2020.csv', 
                 encoding='latin-1',
                 sep=',',  # specify the delimiter (comma in this case)
                 header=0,  # use the first row as the header
                 index_col=None,  # don't use any column as the index
                 on_bad_lines='warn')  # warn about problematic lines
protest_2 = pd.read_csv('ccc_compiled_2021-present.csv', 
                 encoding='latin-1',
                 sep=',',  # specify the delimiter (comma in this case)
                 header=0,  # use the first row as the header
                 index_col=None,  # don't use any column as the index
                 on_bad_lines='warn')  # warn about problematic lines

protests=pd.concat([protest_1, protest_2], axis=0)
protests['date'] = pd.to_datetime(protests['date'])
protests['year'] = protests['date'].dt.year
protests['issues'] = protests['issues'].astype(str)
# Load the data
df = protests.copy() # Replace with your actual file name

# Function to get API key
def get_api_key():
    api_key = os.environ.get("OPENAI_API_KEY")
    if not api_key:
        api_key = 'Api-key'
    return api_key

# Initialize OpenAI client
client = OpenAI(api_key=get_api_key())

# Function to find the main issue
def find_main_issue(group):
    # Clean and split issues
    issues = group['issues'].str.split(';').apply(lambda x: [i.strip() for i in x])
    
    # One-hot encode issues
    mlb = MultiLabelBinarizer()
    issues_encoded = pd.DataFrame(mlb.fit_transform(issues), columns=mlb.classes_, index=group.index)
    
    # Multiply each encoded issue by size_mean
    weighted_issues = issues_encoded.multiply(group['size_mean'], axis=0)
    
    # Calculate the total weight for each issue
    issue_weights = weighted_issues.sum()
    
    # Calculate the frequency of each issue
    issue_frequencies = issues_encoded.sum()
    
    # Combine weight and frequency
    combined_score = issue_weights * issue_frequencies
    
    # Return the issue with the highest combined score
    return combined_score.idxmax()
# Function to classify claims as positive or negative
def classify_claims(claims, issue, client):
    response = client.chat.completions.create(
        model="gpt-4-1106-preview",
        messages=[
            {"role": "system", "content": "You are an AI that classifies claims related to political issues as either positive or negative."},
            {"role": "user", "content": f"Classify the following claims related to the issue '{issue}' with few words.  I.e. main_issue: policing: more policing, 'less policing'. Or military: for peace or for less military - for military. Return only few words for each claim.\n\nClaims:\n{claims}"}
        ],
        max_tokens=4096,
        temperature=0
    )
    
    response_text = response.choices[0].message.content
    
    return response_text

# Apply classification to each row of the full dataframe
def process_row(row):
    classification = classify_claims(row['claims'], row['issues'], client)
    return pd.Series({
        'summary': classification,
    })




df['issues'] = df['issues'].replace('nan', np.nan).str.strip().str.replace(r'\s+', ' ', regex=True)
df = df.dropna(subset=['issues'])

strikes=df[df.type=='strike']
strikes.to_csv('strikes.csv', index=False)

# Group by state and year, and find the main issue
print("Finding main issues for each state and year...")
result = df.groupby(['state', 'year']).apply(find_main_issue).reset_index()
result.columns = ['state', 'year', 'main_issue']
#result.to_csv('results_main_issues.csv')
result=pd.read_csv('results_main_issues.csv')

swing_states=['PA','NV' ,'GA' ,'MI' ,'NC','AZ' ,'WI' ]
main_issues=result.main_issue.unique()
result_swing_states=df[df.state.isin(swing_states)&(df.issues.isin(main_issues))]
# Apply positive/negative estimations to the full dataframe
print("Applying positive/negative estimations to the full dataframe...")
result_swing_states_=result_swing_states.head(10)
result_swing_states_[['summary']] = result_swing_states_.apply(process_row, axis=1)
PA_2017=result_swing_states[(result_swing_states.state=='PA')&(result_swing_states.year==2017)]


# Merge the result with the full dataframe
print("Merging results...")
final_result = result_swing_states.merge(df[['state', 'year', 'positive_count', 'total_count']], on=['state', 'year'])

# Aggregate the positive and total counts if there are multiple entries per state and year
final_result = final_result.groupby(['state', 'year', 'main_issue']).agg({
    'positive_count': 'sum',
    'total_count': 'sum'
}).reset_index()

# Calculate percentages
final_result['positive_issue'] = (final_result['positive_count'] / final_result['total_count']) * 100
final_result['negative_issue'] = 100 - final_result['positive_issue']

# Drop intermediate columns if not needed
final_result = final_result.drop(['positive_count', 'total_count'], axis=1)

# Save the result
print("Saving results to CSV...")
final_result.to_csv('protest_analysis_result.csv', index=False)

print("Analysis complete. Results saved to 'protest_analysis_result.csv'.")