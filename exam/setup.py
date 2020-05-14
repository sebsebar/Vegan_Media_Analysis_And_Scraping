"""For packaging and installation."""

from setuptools import setup
wdir='/Users/FlowersnIce-cream/Google Drev/Hogwarts/First semester/Cognition and communication/CogCom Exam/exam'

setup(
    name='afinn',
    packages=['afinn'],
    version='0.2dev',
    author='Finn Aarup Nielsen',
    author_email='faan@dtu.dk',
    description='AFINN sentiment analysis',
    license='GPL',
    keywords='sentiment analysis',
    url='https://github.com/fnielsen/afinn',
    package_data={'afinn': ['data/*.txt', 'data/LICENSE']},
    long_description='',
    classifiers=[
        'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
        ],
    )
