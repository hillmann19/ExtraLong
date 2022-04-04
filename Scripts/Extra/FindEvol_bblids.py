import flywheel
from datetime import datetime
import pandas as pd

fw = flywheel.Client()
project = fw.projects.find_first('label=Evolution_833922')

subid = []
sesid = []
doscan = []

for sess in project.sessions():
    subid.append(sess.subject.label)
    sesid.append(sess.label)
    doscan.append(sess.timestamp.strftime("%m/%d/%Y"))

Evol_dict = dict()
Evol_dict['subid'] = subid
Evol_dict['sesid'] = sesid
Evol_dict['doscan'] = doscan
Evol_df = pd.DataFrame.from_dict(Evol_dict)
curr_date = datetime.now().strftime("%m_%d_%Y")
Evol_df.to_csv(f"/Users/hillmann/Projects/ExtraLong/Data/FreesurferLongitudinal2021/Evolution_subjects_{curr_date}.csv")