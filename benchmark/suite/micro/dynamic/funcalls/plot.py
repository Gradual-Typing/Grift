import numpy as np
import matplotlib as mpl

latex_output = True

if latex_output:
    mpl.use('PDF')
    mpl.rc('font',**{'family':'serif','serif':['Computer Modern Roman']})
    mpl.rc('text', usetex=True)

mpl.rcParams.update({'font.size': 15})
    
import matplotlib.pyplot  as plt
import matplotlib.patches as mpatch
import matplotlib.lines   as mlines

plt.figure(1)
grift_data = np.genfromtxt('output/call-grift.csv'
                               , delimiter=','
                               , dtype=(int, float, float,float,float)
                               , names=   "n1, coercions_time, csd, type_based_casts_time, tsd"
                               , usecols=("n1, coercions_time, csd, type_based_casts_time, tsd")
                           , skip_header=1)

gambit_data = np.genfromtxt('output/call-gambit.csv'
                               , delimiter=','
                               , dtype=(int, float, float)
                               , names="n2, gambit_time, gsd"
                               , usecols=("n2,gambit_time,gsd")
                            , skip_header=1)

def div(a,b):
    return a/b

vfunc = np.vectorize(div)
x1 = vfunc(grift_data["coercions_time"], gambit_data["gambit_time"])
x2 = vfunc(grift_data["type_based_casts_time"], gambit_data["gambit_time"])

data1 = plt.errorbar(grift_data["n1"]
                  , grift_data["coercions_time"]
                  , grift_data["csd"]
                  , marker='o'
                  , color="blue"
                  , label='Coercions')

data2 = plt.errorbar(grift_data["n1"]
                  , grift_data["type_based_casts_time"]
                      , grift_data["tsd"]
                  , marker='o'
                  , color="red"
                  , label='Type-based casts')

data3, = plt.plot(grift_data["n1"]
                  , gambit_data["gambit_time"]
                  , color="green"
                  , label='Gambit Scheme')

nmin=min(grift_data["n1"])
nmax=max(grift_data["n1"])
nmax2=int(nmax/2)
x=int(max(gambit_data["n2"])/max(grift_data["n1"]))

plt.axis([nmin, nmax, 0, max(np.nanmax(grift_data["coercions_time"]),
                       np.nanmax(grift_data["type_based_casts_time"]),
                       np.nanmax(gambit_data["gambit_time"])) + np.nanmax(grift_data["csd"])+20])
plt.xticks([nmin, nmax/2, nmax],[str(nmin)+'\n'+str(nmin*x),str(nmax2)+'\n'+str(nmax2*x),str(nmax)+'\n'+str(nmax*x)])
plt.xlabel('Number of recursive calls')
plt.ylabel('Time in ns/call')
plt.title('')

plt.legend(loc="upper right"
           , shadow=False
           , prop={'size':10})

if latex_output:
    plt.savefig('output/grift_vs_gambit_funcall_dyn1', bbox_inches='tight')
else:
    plt.show()

plt.figure(1)
plt.gcf().clear()

data1, = plt.plot(grift_data["n1"]
                      , x1
                  , marker='o'
                  , color="blue"
                  , label='Coercions')

data2, = plt.plot(grift_data["n1"]
                  , x2
                  , marker='o'
                  , color="red"
                  , label='Type-based casts')

data3, = plt.plot(grift_data["n1"]
                  , np.array(np.repeat(1,len(x1)))
                  , color="green"
                  , label='Gambit Scheme')

nmin=min(grift_data["n1"])
nmax=max(grift_data["n1"])
nmax2=int(nmax/2)
x=int(max(gambit_data["n2"])/max(grift_data["n1"]))

plt.axis([nmin, nmax, 0, max(np.nanmax(x1),np.nanmax(x2))+.5])
plt.xticks([nmin, nmax2, nmax],[str(nmin)+'\n'+str(nmin*x),str(nmax2)+'\n'+str(nmax2*x),str(nmax)+'\n'+str(nmax*x)])
plt.xlabel('Number of recursive calls')
plt.ylabel('Performance ratio to Gambit-C')
plt.title('')

plt.legend(loc="upper right"
           , shadow=False
           , prop={'size':10})

if latex_output:
    plt.savefig('output/grift_vs_gambit_funcall_dyn2', bbox_inches='tight')
else:
    plt.show()
