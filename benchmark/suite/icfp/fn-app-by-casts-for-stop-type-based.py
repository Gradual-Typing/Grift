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

def sort_for_violin_plot(data_set, key='coercions', value="time"):
    data = {c : [] for c in data_set[key]}
    for datum in data_set:
        data[datum[key]].append(datum[value])
    data = data.items()
    data.sort(key=(lambda x: x[0]))
    return tuple([list(t) for t in zip(*data)])

def set_violin_color_label(vplot, color, label):
    for p in vplot['bodies']:
        p.set_color(color)
    return mpatch.Patch(color=color, label=label)

# Function application analysis on casts
fn_app_casts_data = np.genfromtxt('partially-typed-function-app-results-coercions=15.txt'
                                  , dtype=(int, int, int, float, float, float, float)
                                  , names="casts, types, coercions, trun, titer, crun, citer"
                                  , usecols=("casts, titer, citer"))

# Twosomes function application analysis by casts
tdata_pos, tdata_times = sort_for_violin_plot(fn_app_casts_data, key="casts", value="titer")
tdata_vplt = plt.violinplot(tdata_times
                            , positions=tdata_pos
                            , widths=0.5
                            , showextrema=False
                            , showmedians=False
                            , showmeans=False)
tdata_vplt_fake = set_violin_color_label(tdata_vplt, "blue", "Type-Based Casts")

tdata_lreg = np.polyfit(fn_app_casts_data["casts"]
                        , fn_app_casts_data["titer"]
                        , 1)
tdata_lreg = np.polyfit(fn_app_casts_data["casts"], fn_app_casts_data["titer"], 1)
tdata_lplt = plt.plot(fn_app_casts_data["casts"]
                      , np.polyval(tdata_lreg, fn_app_casts_data["casts"])
                      , linestyle="-"
                      , color="red")

if tdata_lreg[1] < 0 :
    tdata_lplt_eq = "%.2f$N_c$ - %.2f" % (tdata_lreg[0], (-1.0 * tdata_lreg[1]))
else:
    tdata_lplt_eq = "%.2f$N_c$ + %.2f" % (tdata_lreg[0], tdata_lreg[1])

tdata_lplt_fake = mlines.Line2D([],[],color="red", label='Linear model of Type-Based Casts')
tdata_lplt_txt  = plt.text(10, 400 , tdata_lplt_eq, color=tdata_lplt_fake.get_color())

# Coercions cast introduction analysis
cdata_pos, cdata_times = sort_for_violin_plot(fn_app_casts_data, key='casts', value='citer')
# cdata_vplt = plt.violinplot(cdata_times
#                             , positions=cdata_pos
#                             , widths=0.6
#                             , showextrema=False
#                             , showmedians=False
#                             , showmeans=False)
# cdata_vplt_fake = set_violin_color_label(cdata_vplt, "green", "Coercions")

plt.axis([-1, max(cdata_pos) + 1, -10, 1600])
plt.xlabel('Number of casts before function application ($N_c$)')
plt.ylabel('Time in nanoseconds per function application')

# legend = plt.legend(handles=[ tdata_vplt_fake
#                               #,cdata_vplt_fake
# ]
#                     , loc="upper left"
#                     , shadow=True)

if latex_output:
    plt.savefig('fn-app-by-casts-for-stop.pdf', bbox_inches='tight')
else:
    plt.savefig('fn-app-by-casts-for-stop-type-based.png', bbox_inches='tight')
    plt.show()
