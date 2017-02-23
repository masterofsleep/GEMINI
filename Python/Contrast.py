import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
cohort = pd.read_csv("H:/GEMINI/Results/Contrast/cohort.csv")
cci_code = pd.read_excel("H:\GEMINI\Feasibility\Contrast\Contrast CCI Codes.xlsx")
ip_int = pd.read_csv("H:/GEMINI/Data/GEMINI/gim.ip_int.csv")
er_int = pd.read_csv("H:/GEMINI/Data/GEMINI/gim.er_int.csv")
ip_cec = ip_int[ip_int.Diagnosis.Code.isin(c)]