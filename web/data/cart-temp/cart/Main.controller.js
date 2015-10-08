sap.ui.controller("cart.Main", {

	/**
	 * Called when a controller is instantiated and its View controls (if
	 * available) are already created. Can be used to modify the View before it
	 * is displayed, to bind event handlers and do other one-time
	 * initialization.
	 * 
	 * @memberOf cart.Main
	 */
	onInit : function() {
		var oModel = new sap.ui.model.json.JSONModel("cart/products.json");
		this.getView().setModel(oModel);

		var currentCart = localStorage.getItem('currentCart')
		if (currentCart === null) {
			localStorage.setItem('currentCart', JSON.stringify({
				"cart" : [],
				"totalAmount": 0,
				"lastProduct" : undefined,
				"history": []
			}))
		} else {
			console.log("We stored the cart sometime ago, it seems. It is:\n")
			console.log(localStorage.getItem('currentCart'));

			// Reset the last seen product to undefined.
			currentCart = JSON.parse(currentCart);
			currentCart.lastProduct = undefined;
			localStorage.setItem('currentCart', JSON.stringify(currentCart));

		}
	},

	/**
	 * Similar to onAfterRendering, but this hook is invoked before the
	 * controller's View is re-rendered (NOT before the first rendering!
	 * onInit() is used for that one!).
	 * 
	 * @memberOf cart.Main
	 */
	// onBeforeRendering: function() {
	//
	// },
	/**
	 * Called when the View has been rendered (so its HTML is part of the
	 * document). Post-rendering manipulations of the HTML could be done here.
	 * This hook is the same one that SAPUI5 controls get after being rendered.
	 * 
	 * @memberOf cart.Main
	 */
	// onAfterRendering: function() {
	//
	// },
	showCurrentCart : function() {
		var currentCartView = new sap.ui.view({
			id : "currentCart",
			viewName : "cart.CurrentCart",
			type : sap.ui.core.mvc.ViewType.XML
		});

		console.log(currentCartView);

		var mainShell = sap.ui.getCore().byId("mainShell");
		mainShell.setApp(currentCartView);
	},

	onSearch : function(oEvent) {
		
		console.log("On Search Called.");
		
		var filters = [];
		var sQuery = oEvent.getSource().getValue();

		var productsView = this.byId("productsView");
		var selectList = productsView.byId("selectList");
		var binding = selectList.getBinding("items");
		
		console.log("Binding is: ");
		console.log(binding);
		
		if (sQuery && sQuery.length > 0) {
			var filter = new sap.ui.model.Filter("Name",
					sap.ui.model.FilterOperator.Contains, sQuery);
			filters.push(filter);
		}
		
		binding.filter(filters);
	},
/**
 * Called when the Controller is destroyed. Use this one to free resources and
 * finalize activities.
 * 
 * @memberOf cart.Main
 */
// onExit: function() {
//
// }
});