# coding= utf-8
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

# Function cast analysis
plt.figure(1)
loop_data = np.genfromtxt('loop-fn-app.txt',
                          dtype=(int, float, float),
                          names="apps, t_time, c_time",
                          usecols=("apps, t_time, c_time"))

def violin_plot_data(data, x_key, y_key, color, name
                     , violin_widths=.35
                     , violin_showextrema=False
                     , violin_showmedians=False
                     , violin_showmeans=False
                     , reg_poly=0
                     , reg_color="red"
                     , reg_linstyle="-"):
    x_vals, y_vals = sort_for_violin_plot(data , key=x_key , value=y_key)
    vplt = plt.violinplot(tdata_times
                          , positions=x_vals
                          , widths=violin_widths
                          , showextrema=violin_showextrema
                          , showmedians=violin_showmedians
                          , showmeans=violin_showmeans)
    vplt_fake = set_violin_color_label(vplt, color, name)
    if 0 == reg_poly:
        return vplt_fake
    else:
        reg  = np.polyfit(data[x_key], data[y_key], reg_poly)
        rplt = plt.plot(data[x_key]
                        , np.polyval(reg, loop_data[x_key])
                        , linestyle=reg_linestyle
                        , color=reg_color)
        if reg[-1] < 0 :
            eq = "%.2f$A$ - %.2f\t" % (reg[0], (-1.0 * reg[1]))
        else:
            eq = "%.2f$A$ + %.2f\t" % (reg[0], reg[1])
        reg_fake = mlines.Line2D([],[],color=reg_color, label=reg_label)
        if reg_text:
            reg_eq_txt  = plt.text(reg_label, 8 , cdata_lplt_eq, color=cdata_lplt_fake.get_color())
        return vplt_fake, reg_fake

# Twosomes cast introduction analysis
tdata_pos, tdata_times = sort_for_violin_plot(loop_data
                                              , key='apps'
                                              , value='t_time')
tdata_vplt = plt.violinplot(tdata_times
                            , positions=tdata_pos
                            , widths=.35
                            , showextrema=False
                            , showmedians=False
                            , showmeans=False)
tdata_vplt_fake = set_violin_color_label(tdata_vplt, "blue", "Type-Based Casts")

# tdata_lreg = np.polyfit(twosomes_cast_data["apps"]
#                         ,twosomes_cast_data["t_time"]
#                         ,1)

# Coercions cast introduction analysis
cdata_pos, cdata_times = sort_for_violin_plot(loop_data
                                              , key='apps'
                                              , value='t_time')
cdata_vplt = plt.violinplot(cdata_times
                            , positions=cdata_pos
                            , widths=.35
                            , showextrema=False
                            , showmedians=False
                            , showmeans=False)
cdata_vplt_fake = set_violin_color_label(cdata_vplt, "green", "Coercions")


cdata_lreg = np.polyfit(loop_data["apps"], loop_data["c_time"], 1)
cdata_lplt = plt.plot(loop_data["apps"]
                      , np.polyval(cdata_lreg, loop_data["apps"])
                      , linestyle="-"
                      , color="red")

if cdata_lreg[1] < 0 :
    cdata_lplt_eq = "%.2f$A$ - %.2f" % \
                    (cdata_lreg[0], (-1.0 * cdata_lreg[1]))
else:
    cdata_lplt_eq = "%.2f$A$ + %.2f" % (cdata_lreg[0], cdata_lreg[1])
cdata_lplt_fake = mlines.Line2D([],[],color="red", label='Linear model of Coercions')
cdata_lplt_txt  = plt.text(5, 8 , cdata_lplt_eq, color=cdata_lplt_fake.get_color())

plt.axis([0, 16, 0, 50])
plt.xlabel('Number of Applications of Function w/o Proxy (A)')
plt.ylabel('Time in $ns$ per iteration')
#plt.title('Timing Results for Function Casts Resulting in Higher-Order Casts')

legend = plt.legend(handles=[ tdata_vplt_fake
                              ,cdata_vplt_fake
                              #,cdata_lplt_fake
]
                    , loc="upper left"
                    , shadow=True)

if latex_output:
    plt.savefig('loop-fn-app', bbox_inches='tight')
else:
    plt.savefig('loop-fn-app.png', bbox_inches='tight')
    plt.show() 


