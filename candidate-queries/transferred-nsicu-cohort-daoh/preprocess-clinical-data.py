# Load dependencies
import pandas as pd
import numpy as np
import sqlalchemy
from sqlalchemy import create_engine, text

# Define the SQLalchemy engine
engine = create_engine(f"sqlite:////Users/JO/PhD/datauttag/2025/db_2025.sqlite")

with open('/Users/JO/PhD/neuro-ascertainment/candidate-queries/transferred-nsicu-cohort-daoh/transferred-cohort-daoh.sql', 'r') as file:
    query = file.read()

d = pd.read_sql(query + "SELECT * FROM FINAL", engine)
d.to_csv("d.csv", sep=";")