class Foo(object):

    def __add__(self,that):
        raise "ehll"

    def __getattribute__(self,attr):
        print "ga " + attr
        return super(Foo,self).__getattribute__(attr)

foo = Foo()
# print foo.__add__
# print foo.__class__.__add__
print foo + 0
# print foo.__add__(0)
