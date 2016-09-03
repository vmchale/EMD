import haskemd

a = haskemd.equalize(haskemd.rand(1023),haskemd.mrand(1023))

print (haskemd.emd(a[0],a[1]))
print("...test completed sucessfully!")
