import numpy as np
import matplotlib as mpl

latex_output = False

if latex_output:
    mpl.use('PDF')
    mpl.rc('font',**{'family':'serif','serif':['Computer Modern Roman']})
    mpl.rc('text', usetex=True)

mpl.rcParams.update({'font.size': 15})
    
import matplotlib.pyplot  as plt
import matplotlib.patches as mpatch
import matplotlib.lines   as mlines

def linear_reg(xs, ys):
    a, b = np.polyfit(xs, ys, 1)
    
    fs = np.polyval([a,b], xs)
    
    mean_y = np.sum(ys) / len(ys)
    
    ss_regression = np.sum((fs - mean_y) ** 2)
    ss_total = np.sum((ys - mean_y) ** 2)
    r2 = ss_regression / ss_total
    
    return a, b , r2

# Function cast analysis
plt.figure(1)
cast_data = np.genfromtxt('partially-typed-function-cast.txt',
                                   dtype=(int, float, float),
                                   names="size, coercions, type_based")

# Twosomes cast introduction analysis
cast_tb_points = plt.plot(cast_data["size"], cast_data["type_based"]
                          , color='blue'
                          , label="Type-Based Casts"
                          , linestyle='None'
                          , marker='o')

tlreg = linear_reg(cast_data["size"], cast_data["type_based"])
print("type-based a=%f b=%f r2=%f" % tlreg) 
tdata_lreg = np.polyfit(cast_data["size"] , cast_data["type_based"] , 1)
tdata_lplt = plt.plot(cast_data["size"]
                      , np.polyval(tdata_lreg, cast_data["size"])
                      , linestyle="-"
                      , color="blue"
                      , label="Linear Model of Type-Based Casts")

# Coercions cast introduction analysis
cast_c_points = plt.plot(cast_data["size"], cast_data["coercions"]
                         , color='green'
                         , label="Coercions"
                         , linestyle='None'
                         , marker='x')

clreg = linear_reg(cast_data["size"], cast_data["coercions"])
print("coercions a=%f b=%f r2=%f" % clreg) 
cdata_lreg = np.polyfit(cast_data["size"], cast_data["coercions"], 1)
cdata_lplt = plt.plot(cast_data["size"]
                      , np.polyval(cdata_lreg, cast_data["size"])
                      , linestyle="-"
                      , color="green"
                      , label="Linear Model of Coercions")

# if cdata_lreg[1] < 0 :
#     cdata_lplt_eq = "%.2f$N_c$ - %.2f" % (cdata_lreg[0], (-1.0 * cdata_lreg[1]))
# else:
#     cdata_lplt_eq = "%.2f$N_c$ + %.2f" % (cdata_lreg[0], cdata_lreg[1])
# cdata_lplt_fake = mlines.Line2D([],[],color="red", label='Linear model of Coercions')
# cdata_lplt_txt  = plt.text(80, 1200 , cdata_lplt_eq, color=cdata_lplt_fake.get_color())

#plt.axis([0, max(cdata_pos) + 10, -250, 4000])
plt.xlabel('Number of nodes in Coercion resulting from types ($N_c$)')
plt.ylabel('nanoseconds per cast')
#plt.title('Timing Results for Function Casts Resulting in Higher-Order Casts')

legend = plt.legend(loc="upper left" , shadow=True)

if latex_output:
    plt.savefig('partially-typed-fn-cast-results', bbox_inches='tight')
else:
    plt.savefig('partially-typed-fn-cast-results.png', bbox_inches='tight')
    plt.show()
