from setuptools import setup

setup(name='essen_table_converter',
      version='0.1',
      description='Find information about games in BGG and add it to google sheet'
      long_description='input is google sheet where games which are intersted are listed in teh 1st column with their BGG link, output is a new sheet in teh table conaining information collected from BGG'
      classifiers=[
          'Development Status :: 4 - Beta',
          'Programming Language :: Python :: 2.7',
          'Topic :: Games/Entertainment :: Board Games',
      ],
      keywords='bgg essen 2016 gspread',
      url='http://github.com/evabihari/bgg_feed',
      author='Eva Bihari',
      author_email='enzian.h@gmail.com',
      packages=['essen_table_converter'),
      install_requirements=[
          'gspread',
          're',
          'boardgamegeek',
          'oauth2client',
      ],
      include_package_data=True,
      zip_safe=False)
          
