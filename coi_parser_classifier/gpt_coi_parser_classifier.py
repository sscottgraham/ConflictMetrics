import openai
import polars as pl
import json

df = pl.read_csv('')

client = openai.OpenAI()
join_list = []

for row in df.with_row_index().rows():
    completion = client.chat.completions.create(
        model='gpt-4-0125-preview',
        messages=[
            {"role": "system", "content": "You are an expert medical writer with over a decade of experience. \
            Your job to provide answers about information in medical texts."
             },
            {"role": "user", "content": "In the following phrase, first, identify all people's names in the sentence.\
            Be aware that people's names will often appear as initials). \
            Then, identify separately each <source> of funding for each name. This is usually a company, \
            but it can also be a university, government agency, or professional society. \
            Finally, identify the <type> of funding relationship. Types of funding relationships should be standardized to the following:\
            research (which includes grants and sponsored clinical trials), \
            employment, \
            consulting, \
            advisory (which includes member, paid advice, membership on boards, and committees), \
            speaking (which includes lectures, speaker fees, paid presentations, and honoraria), \
            unspecified fees (which includes contributions and personal fees), \
            travel (which includes accommodation), \
            stock (which includes shareholder, equity, interest, dividends), \
            patents (which includes inventions, IP, and copyright; note that patents have no <source>), \
            and 'other' for anything that doesn't fit (like ownership, royalties, or editorship). \
            For each name, report each source of funding and its corresponding funding relationship in a different row. \
            Output these in JSON format, using the angled brackets above as keys, like so: '(<name>:name, <source>:source, <type>:type).\
            Here's the phrase: {}".format(row[3])
             }
        ]
    )
    join_list.append((row[0], completion.choices[0].message.content))

df_temp = pl.DataFrame(join_list)
df_temp = df_temp.rename({'column_0':'index', 'column_1':'coi_parsed'})
df_temp = df_temp.select(
    pl.col('index').cast(pl.UInt32),
    pl.col('coi_parsed')
)


clean_df = df.with_row_index().join(df_temp, on='index')
clean_df.write_csv('')
