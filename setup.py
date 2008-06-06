from distutils.core import setup
setup( name='dramatis',
       version='0.1.1',
       author='Steven Parkes',
       author_email='smparkes@smparkes.net',
       url='http://dramatis.mischance.net',
       description="an actor library for ruby and python",
       package_dir = {'':'lib'},
       packages=[
               'dramatis',
               'dramatis.error',
               'dramatis.future_value',
               'dramatis.actor',
               'dramatis.actor.name',
               'dramatis.runtime',
               'dramatis.runtime.actor',
               'dramatis.runtime.continuation',
               ],
       )
