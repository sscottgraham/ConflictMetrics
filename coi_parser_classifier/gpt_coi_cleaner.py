import openai
import polars as pl

df = pl.read_csv('all_sponsors_unique.csv')

client = openai.OpenAI()

join_list = []
for row in df.rows():
    completion = client.chat.completions.create(
        model='gpt-4-0125-preview',
        messages=[
            {"role": "system", "content": "You are an expert medical writer with over a decade of experience. \
            Your job to provide answers about information in medical texts."},
            {"role":"user", "content": "You will be provided with a company or organization name in the biomedical sector. \
            Please clean this name to a standardized name, translated to English, for the organizational entity. \
            Only report back the standardized name, with no other conversational text around it. \
            In addition, classify the entity as one of the following: industry, federal agency, patient advocacy organization, hospital/clinic, university, philantropic foundation, or professional society/academic journal. \
            Please output these in JSON format, like so: ('standardized name': standardized name, 'classification': classification)\
            Here is the company or organization name for you to convert: {}".format(row[0])}
        ]
    )
    join_list.append((row, completion.choices[0].message.content))


df_temp = pl.DataFrame(join_list)
df_temp = df_temp.rename({'column_0':'source', 'column_1':'cleaned_json'})
df_temp.write_excel('cleaned_sponsors_all.csv')
