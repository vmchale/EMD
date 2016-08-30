import haskemd

a = haskemd.equalize(haskemd.sinksrand(1023),haskemd.sourcesrand(1023))

print (haskemd.emd(a[0],a[1]))
print("...test completed sucessfully!")
