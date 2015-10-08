sap.ui.controller("cart.ProductDetails", {
	onInit : function() {
		var oData = {
			Name : "Welcome!",
			Price : "",
			Description : "Please select an item from the list.",
			ProductPicUrl : "sap-icon://arrow-left"
		};

		var oModel = sap.ui.model.json.JSONModel(oData);
		this.getView().setModel(oModel);
	},

	onBuyButtonPress : function() {
		var currentCart = JSON.parse(localStorage.getItem('currentCart'));
		var lastProduct = currentCart.lastProduct;

		if (lastProduct === undefined) {
			sap.m.MessageToast.show("Please select an item first.", {
				duration : 3000,
				at : "Center Top",
				width : "40%"

			});
			return;
		}
		// Append the new product to the cart list
		currentCart.cart.push(lastProduct);
		var totalAmount = currentCart.totalAmount + lastProduct.Price;
		currentCart.totalAmount = parseFloat(totalAmount.toFixed(2));

		localStorage.setItem('currentCart', JSON.stringify(currentCart));

		sap.m.MessageToast.show(lastProduct.Name + " added to cart", {
			duration : 3000,
			at : "Center Center",
			width : "40%"
		});
		console.log(lastProduct);
	}
});