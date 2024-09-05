// Generated by js_of_ocaml
//# buildInfo:effects=false, kind=cmo, use-js-string=true, version=5.4.0

//# unitInfo: Provides: Dune__exe__NcaSpletniVmesnik
//# unitInfo: Requires: Definicije__Cell, Dune__exe__NcaModel, Dune__exe__NcaView, Js_browser, Stdlib__array, Stdlib__list, Stdlib__option, Vdom, Vdom_blit
(function
  (globalThis){
   "use strict";
   var runtime = globalThis.jsoo_runtime;
   function caml_call1(f, a0){
    return (f.l >= 0 ? f.l : f.l = f.length) == 1
            ? f(a0)
            : runtime.caml_call_gen(f, [a0]);
   }
   function caml_call2(f, a0, a1){
    return (f.l >= 0 ? f.l : f.l = f.length) == 2
            ? f(a0, a1)
            : runtime.caml_call_gen(f, [a0, a1]);
   }
   function caml_call3(f, a0, a1, a2){
    return (f.l >= 0 ? f.l : f.l = f.length) == 3
            ? f(a0, a1, a2)
            : runtime.caml_call_gen(f, [a0, a1, a2]);
   }
   function caml_call4(f, a0, a1, a2, a3){
    return (f.l >= 0 ? f.l : f.l = f.length) == 4
            ? f(a0, a1, a2, a3)
            : runtime.caml_call_gen(f, [a0, a1, a2, a3]);
   }
   var
    global_data = runtime.caml_get_global_data(),
    Js_browser = global_data.Js_browser,
    Stdlib_array = global_data.Stdlib__array,
    Stdlib_option = global_data.Stdlib__option,
    Vdom_blit = global_data.Vdom_blit,
    Definicije_Cell = global_data.Definicije__Cell,
    Stdlib_list = global_data.Stdlib__list,
    Dune_exe_NcaModel = global_data.Dune__exe__NcaModel,
    Dune_exe_NcaView = global_data.Dune__exe__NcaView,
    Vdom = global_data.Vdom,
    cst_container = "container",
    cst_div = "div",
    _a_ = [0, 0., 0., 0.],
    _b_ = [0, 1., 0., 1.];
   function custom_update_rule(cell, neighbors){
    function _h_(neighbor){
     return caml_call1(Definicije_Cell[3], neighbor) < 0.1 ? 1 : 0;
    }
    if(! caml_call2(Stdlib_list[31], _h_, neighbors)) return cell;
    var _i_ = caml_call2(Definicije_Cell[6], 0., cell);
    return caml_call2(Definicije_Cell[5], _a_, _i_);
   }
   var
    _c_ = caml_call3(Definicije_Cell[1], _b_, 1., [0]),
    init_model =
      caml_call4(Dune_exe_NcaModel[1], 250, 250, _c_, custom_update_rule),
    app =
      caml_call4
       (Vdom[64], init_model, Dune_exe_NcaModel[2], Dune_exe_NcaView[3], 0);
   function run(param){
    var
     _d_ = caml_call2(Js_browser[16][8], Js_browser[26], cst_container),
     param$0 = caml_call1(Stdlib_array[11], _d_);
    if(param$0) var x = param$0[1], _e_ = [0, x]; else var _e_ = 0;
    var
     func = Stdlib_option[3],
     container =
       caml_call2
        (func, _e_, caml_call2(Js_browser[16][3], Js_browser[26], cst_div)),
     _f_ = caml_call3(Vdom_blit[11], 0, 0, app),
     _g_ = caml_call1(Vdom_blit[13], _f_);
    return caml_call2(Js_browser[15][9], container, _g_);
   }
   caml_call2(Js_browser[19][6], Js_browser[25], run);
   var Dune_exe_NcaSpletniVmesnik = [0];
   runtime.caml_register_global
    (16, Dune_exe_NcaSpletniVmesnik, "Dune__exe__NcaSpletniVmesnik");
   return;
  }
  (globalThis));

//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLjAsImZpbGUiOiIubmNhU3BsZXRuaVZtZXNuaWsuZW9ianMvanNvby9kdW5lX19leGVfX05jYVNwbGV0bmlWbWVzbmlrLmNtby5qcyIsInNvdXJjZVJvb3QiOiIiLCJuYW1lcyI6WyJjdXN0b21fdXBkYXRlX3J1bGUiLCJjZWxsIiwibmVpZ2hib3JzIiwibmVpZ2hib3IiLCJpbml0X21vZGVsIiwiYXBwIiwicnVuIiwieCIsImNvbnRhaW5lciJdLCJzb3VyY2VzIjpbIi93b3Jrc3BhY2Vfcm9vdC9zcmMvbmNhU3BsZXRuaVZtZXNuaWsvbmNhU3BsZXRuaVZtZXNuaWsubWwiXSwibWFwcGluZ3MiOiI7Ozs7Ozs7OztHOzs7OztHOzs7OztHOzs7OztHOzs7Ozs7Ozs7Ozs7Ozs7Ozs7OztZQU1JQSxtQkFBbUJDLE1BQUtDO2lCQUNOQztLQUFZLE9BQUEsK0JBQVpBO0lBQXlDO0lBQTFELEtBQUEsaUNBRHVCRCxZQUl4QixPQUptQkQ7SUFFVSxVQUFBLG1DQUZWQTtJQUVVLE9BQUE7R0FFekI7R0FLZ0I7SUFBQSxNQUFBO0lBRHBCRztNQUNGLGdEQVRFSjtJQWlCQUs7TUFDRjtrQkFWRUQ7WUFtQkVFO0lBQ0Y7S0FDRSxNQUFBO0tBQUEsVUFBQTtvQkFmRkMsMEJBQUFBO0lBZUU7S0FBQTtLQURFQztPQUl1QjtvQkFBQTtLQUUzQixNQUFBLGdDQWpCQUg7S0FpQkEsTUFBQTtJQUFBLE9BQUEsOEJBTklHO0dBUXdDO0dBRTlDLDhDQVhJRjs7Ozs7RSIsInNvdXJjZXNDb250ZW50IjpbIm9wZW4gRGVmaW5pY2lqZVxuXG4oKiBEZWZpbmUgYSBuby1vcCB1cGRhdGUgcnVsZSAqKVxubGV0IF9kb19ub3RoaW5nIGNlbGwgX25laWdoYm9ycyA9IGNlbGxcblxuKCogRGVmaW5lIHRoZSB1cGRhdGUgcnVsZSAqKVxubGV0IGN1c3RvbV91cGRhdGVfcnVsZSBjZWxsIG5laWdoYm9ycyA9XG4gIGlmIExpc3QuZXhpc3RzIChmdW4gbmVpZ2hib3IgLT4gQ2VsbC5nZXRfYWxwaGEgbmVpZ2hib3IgPCAwLjEpIG5laWdoYm9ycyB0aGVuXG4gICAgQ2VsbC5zZXRfcmdiICgwLjAsIDAuMCwgMC4wKSAoQ2VsbC5zZXRfYWxwaGEgMC4wIGNlbGwpXG4gIGVsc2VcbiAgICBjZWxsXG5cblxuKCogSW5pdGlhbGl6ZSB0aGUgbW9kZWwgKilcbmxldCBpbml0X21vZGVsID1cbiAgTmNhTW9kZWwuaW5pdCAyNTAgMjUwIChDZWxsLmluaXQgKDEuMCwgMC4wLCAxLjApIDEuMCBbfHxdKSBjdXN0b21fdXBkYXRlX3J1bGVcblxuKCogSGVscGVyIGZ1bmN0aW9uIHRvIGdldCB0aGUgZmlyc3QgZWxlbWVudCBvZiBhIGxpc3QsIHNhZmVseSAqKVxubGV0IGhkX29wdCA9IGZ1bmN0aW9uXG4gIHwgW10gLT4gTm9uZVxuICB8IHggOjogXyAtPiBTb21lIHhcblxuKCogRGVmaW5lIHRoZSBhcHAgKilcbmxldCBhcHAgPVxuICBWZG9tLnNpbXBsZV9hcHBcbiAgICB+aW5pdDppbml0X21vZGVsXG4gICAgfnZpZXc6TmNhVmlldy52aWV3XG4gICAgfnVwZGF0ZTpOY2FNb2RlbC51cGRhdGVcbiAgICAoKVxuXG4oKiBSdW4gdGhlIGFwcGxpY2F0aW9uICopXG5sZXQgKCkgPVxuICBsZXQgb3BlbiBKc19icm93c2VyIGluXG4gIGxldCBydW4gKCkgPVxuICAgIGxldCBjb250YWluZXIgPVxuICAgICAgSnNfYnJvd3Nlci5Eb2N1bWVudC5nZXRfZWxlbWVudHNfYnlfY2xhc3NfbmFtZSBkb2N1bWVudCBcImNvbnRhaW5lclwiXG4gICAgICB8PiBBcnJheS50b19saXN0XG4gICAgICB8PiBoZF9vcHRcbiAgICAgIHw+IE9wdGlvbi52YWx1ZSB+ZGVmYXVsdDooSnNfYnJvd3Nlci5Eb2N1bWVudC5jcmVhdGVfZWxlbWVudCBkb2N1bWVudCBcImRpdlwiKVxuICAgIGluXG4gICAgVmRvbV9ibGl0LnJ1biBhcHBcbiAgICB8PiBWZG9tX2JsaXQuZG9tXG4gICAgfD4gSnNfYnJvd3Nlci5FbGVtZW50LmFwcGVuZF9jaGlsZCBjb250YWluZXJcbiAgaW5cbiAgV2luZG93LnNldF9vbmxvYWQgd2luZG93IHJ1blxuIl19
