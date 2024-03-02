from setuptools import setup

APP=['simple_gui_interface.py']
OPTIONS={
    'argv_emulation': True
}

setup(app=APP, options={'py2app': OPTIONS}, setup_requires=['py2app'])

# Enter in Terminal: python setup.py py2app