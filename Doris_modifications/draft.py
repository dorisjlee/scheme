def scm_to_py(lst):
	if lst == nil:
		return []
	for ele in lst:
		print ("here")
		if ele == nil: #or ele==None:
			print ("inside")
			return []
		else:
			print ("pass")
			return [lst.first]+scm_to_py(lst.second)


py_args=scm_to_py(args)



    >>> env = create_global_frame()
    >>> plus = env.bindings["+"]
    >>> twos = Pair(2, Pair(2, nil))
    >>> apply_primitive(plus, twos, env)
    4
    def scm_to_py(lst):
        for ele in lst:
            if ele == nil or ele==None:
                return []
            else:
                return [lst.first]+scm_to_py(lst.second)
##########final fix###################


def scm_to_py(lst):
	if lst == nil:
		return []
	for ele in lst:
		return [lst.first]+scm_to_py(lst.second)