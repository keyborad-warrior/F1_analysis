import os
import kagglehub
import pandas as pd
import streamlit as st
import plotly.express as px

st.set_page_config(page_title="F1 Pit Stop Analysis", page_icon="🏎️", layout="wide")

@st.cache_data
def load_data():
    path = kagglehub.dataset_download("rohanrao/formula-1-world-championship-1950-2020")
    pit = pd.read_csv(os.path.join(path, "pit_stops.csv"))
    races = pd.read_csv(os.path.join(path, "races.csv"))
    drivers = pd.read_csv(os.path.join(path, "drivers.csv"))
    return pit, races, drivers

pit, races, drivers = load_data()
pit = pit.merge(races[["raceId", "year", "name"]], on="raceId", how="left")
pit = pit.merge(drivers[["driverId", "forename", "surname"]], on="driverId", how="left")
pit["driverName"] = pit["forename"] + " " + pit["surname"]
pit["duration_seconds"] = pd.to_numeric(pit["duration"], errors="coerce")

st.title("🏎️ Formula 1 Pit Stop Analysis")
st.caption("Historical F1 pit-stop analytics using Python, Pandas, Plotly and Streamlit.")

years = st.slider("Season", int(pit.year.min()), int(pit.year.max()),
                  (int(pit.year.min()), int(pit.year.max())))
filtered = pit[pit.year.between(*years)].copy()

c1, c2, c3 = st.columns(3)
c1.metric("Pit Stops", f"{len(filtered):,}")
c2.metric("Average Duration", f"{filtered.duration_seconds.mean():.2f} s")
c3.metric("Fastest Stop", f"{filtered.duration_seconds.min():.2f} s")

st.subheader("Average Pit Stop Duration by Year")
trend = filtered.groupby("year", as_index=False)["duration_seconds"].mean()
st.plotly_chart(px.line(trend, x="year", y="duration_seconds", markers=True,
                        labels={"duration_seconds":"Average Duration (seconds)"}),
                use_container_width=True)

st.subheader("Fastest Pit Stops")
fastest = filtered[["year","name","driverName","lap","duration_seconds"]].dropna().sort_values("duration_seconds").head(20)
st.dataframe(fastest, use_container_width=True)

st.subheader("Pit Stops by Driver")
counts = filtered.groupby("driverName").size().reset_index(name="pit_stops").sort_values("pit_stops", ascending=False).head(15)
st.plotly_chart(px.bar(counts, x="pit_stops", y="driverName", orientation="h"),
                use_container_width=True)
