import numpy as np
import matplotlib as mpl

latex_output = True

if latex_output:
    mpl.use('PDF')
    mpl.rc('font',**{'family':'serif','serif':['Computer Modern Roman']})
    mpl.rc('text', usetex=True)

import matplotlib.pyplot  as plt
import matplotlib.patches as mpatch
import matplotlib.lines   as mlines

# Function cast analysis
plt.figure(1)
twosomes_cast_data = np.genfromtxt('partially-typed-function-cast-twosomes.txt',
                              dtype=(int, int, float),
                              names="types, coercions, time",
                              usecols=("coercions, time"))
coercions_cast_data = np.genfromtxt('partially-typed-function-cast-coercions.txt',
                               dtype=(int, int, float),
                               names=("types", "coercions", "time"),
                               usecols=("coercions", "time"))

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


# Twosomes cast introduction analysis
tdata_pos, tdata_times = sort_for_violin_plot(twosomes_cast_data)
tdata_vplt = plt.violinplot(tdata_times
                            , positions=tdata_pos
                            , widths=7
                            , showextrema=False
                            , showmedians=False
                            , showmeans=False)
tdata_vplt_fake = set_violin_color_label(tdata_vplt, "blue", "Twosomes Represention")

tdata_lreg = np.polyfit(twosomes_cast_data["coercions"]
                        , twosomes_cast_data["time"]
                        , 1)



# Coercions cast introduction analysis
cdata_pos, cdata_times = sort_for_violin_plot(coercions_cast_data)
cdata_vplt = plt.violinplot(cdata_times
                            , positions=cdata_pos
                            , widths=7
                            , showextrema=False
                            , showmedians=False
                            , showmeans=False)
cdata_vplt_fake = set_violin_color_label(cdata_vplt, "green", "Coercions Representation")


cdata_lreg = np.polyfit(coercions_cast_data["coercions"], coercions_cast_data["time"], 1)
cdata_lplt = plt.plot(coercions_cast_data["coercions"]
                      , np.polyval(cdata_lreg, coercions_cast_data["coercions"])
                      , linestyle="-"
                      , color="red")
cdata_lplt_eq = "%f$N_c$ + %f" % (cdata_lreg[0], cdata_lreg[1]) 
cdata_lplt_fake = mlines.Line2D([],[],color="red", label='Linear model of Coercions Representation')
cdata_lplt_txt  = plt.text(80, 1.2 , cdata_lplt_eq, color=cdata_lplt_fake.get_color())


plt.xlabel('Number of nodes in Coercion resulting from types ($N_c$)')
plt.ylabel('Time in $\mu s$ per iteration')
plt.title('Timing Results for Function Casts Resulting in Higher-Order Casts')

legend = plt.legend(handles=[ tdata_vplt_fake
                              ,cdata_vplt_fake
                              ,cdata_lplt_fake]
                    , loc="upper left"
                    , shadow=True)

if latex_output:
    plt.savefig('partially-typed-fn-cast-results', bbox_inches='tight')


# Function application analysis by casts
plt.figure(2)
fn_app_casts_data = np.genfromtxt('partially-typed-function-app-results-coercions=63.txt',
                                  dtype=(int, int, int, float, float, float, float),
                                  names="casts, types, coercions, trun, titer, crun, citer",
                                  usecols=("casts, titer, citer"))

# Twosomes function application analysis by casts
tdata_pos, tdata_times = sort_for_violin_plot(fn_app_casts_data, key="casts", value="titer")
tdata_vplt = plt.violinplot(tdata_times
                            , positions=tdata_pos
                            , widths=0.5
                            , showextrema=False
                            , showmedians=False
                            , showmeans=False)
tdata_vplt_fake = set_violin_color_label(tdata_vplt, "blue", "Twosomes Represention")

tdata_lreg = np.polyfit(fn_app_casts_data["casts"]
                        , fn_app_casts_data["titer"]
                        , 1)
tdata_lreg = np.polyfit(fn_app_casts_data["casts"], fn_app_casts_data["titer"], 1)
tdata_lplt = plt.plot(fn_app_casts_data["casts"]
                      , np.polyval(tdata_lreg, fn_app_casts_data["casts"])
                      , linestyle="-"
                      , color="red")
tdata_lplt_eq = "%f$N_i$ + %f" % (tdata_lreg[0], tdata_lreg[1]) 
tdata_lplt_fake = mlines.Line2D([],[],color="red", label='Linear model of Twosomes Representation')
tdata_lplt_txt  = plt.text(2.3, 0.1 , tdata_lplt_eq, color=tdata_lplt_fake.get_color())


# Coercions cast introduction analysis
cdata_pos, cdata_times = sort_for_violin_plot(fn_app_casts_data, key='casts', value='citer')
cdata_vplt = plt.violinplot(cdata_times
                            , positions=cdata_pos
                            , widths=0.5
                            , showextrema=False
                            , showmedians=False
                            , showmeans=False)
cdata_vplt_fake = set_violin_color_label(cdata_vplt, "green", "Coercions Representation")

plt.axis([-1, 10, -0.2, 2.0])
plt.xlabel('Number of function casts before the application ($N_i$)')
plt.ylabel('Time in $\mu s$ per iteration')
plt.title('Timing Results for Casted Function Application with Fixed Type Size')

legend = plt.legend(handles=[ tdata_vplt_fake
                              ,cdata_vplt_fake
                              ,cdata_lplt_fake]
                    , loc="upper left"
                    , shadow=True)

if latex_output:
    plt.savefig('partially-typed-fn-app-by-casts', bbox_inches='tight')



# Fn app by coercion size
plt.figure(3)
fn_app_coercions_data = np.genfromtxt('partially-typed-function-app-results-casts=1.txt',
                                      dtype=(int, int, int, float, float, float, float),
                                      names="casts, types, coercions, trun, titer, crun, citer",
                                      usecols=("coercions, titer, citer"))

# Twosomes function application analysis by casts
tdata_pos, tdata_times = sort_for_violin_plot(fn_app_coercions_data, key="coercions", value="titer")
tdata_vplt = plt.violinplot(tdata_times
                            , positions=tdata_pos
                            , widths=5
                            , showextrema=False
                            , showmedians=False
                            , showmeans=False)
tdata_vplt_fake = set_violin_color_label(tdata_vplt, "blue", "Twosomes Represention")

# Coercions cast introduction analysis
cdata_pos, cdata_times = sort_for_violin_plot(fn_app_coercions_data, key='coercions', value='citer')
cdata_vplt = plt.violinplot(cdata_times
                            , positions=cdata_pos
                            , widths=5
                            , showextrema=False
                            , showmedians=False
                            , showmeans=False)
cdata_vplt_fake = set_violin_color_label(cdata_vplt, "green", "Coercions Representation")
cdata_lreg = np.polyfit(fn_app_coercions_data["coercions"]
                        , fn_app_coercions_data["titer"]
                        , 1)
cdata_lreg = np.polyfit(fn_app_coercions_data["coercions"], fn_app_coercions_data["citer"], 1)
cdata_lplt = plt.plot(fn_app_coercions_data["coercions"]
                      , np.polyval(cdata_lreg, fn_app_coercions_data["coercions"])
                      , linestyle="-"
                      , color="red")
cdata_lplt_eq = "%f$N_c$ + %f" % (cdata_lreg[0], cdata_lreg[1]) 
cdata_lplt_fake = mlines.Line2D([],[],color="red", label='Linear model of Coercions Representation')
cdata_lplt_txt  = plt.text(35, 0.15 , cdata_lplt_eq, color=cdata_lplt_fake.get_color())



plt.xlabel('Number of nodes in the Coercions resulting from the types ($N_c$)')
plt.ylabel('Time in $\mu s$ per iteration')
plt.title('Timing Results for Application of a Function Casted Once')

legend = plt.legend(handles=[ tdata_vplt_fake
                              ,cdata_vplt_fake
                              ,tdata_lplt_fake]
                    , loc="upper left"
                    , shadow=True)

if latex_output:
    plt.savefig('partially-typed-fn-app-by-types', bbox_inches='tight')
else:
    plt.show()
