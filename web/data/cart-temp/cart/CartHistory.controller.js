sap.ui.controller("cart.CartHistory", {
	onInit : function() {
		var currentCart = JSON.parse(localStorage.getItem("currentCart"))
		console.log(currentCart);
		
		if (currentCart != null) {
			console.log("Setting model on CartHistory view now.");
			var oModel = new sap.ui.model.json.JSONModel(currentCart);
			this.getView().setModel(oModel);
		} else {
			sap.m.MessageToast.show("Sorry. There is no history so far.", {
				duration : 3000
			});
		}
	},
	
	onHomeButtonPress: function () {
		location.reload();
	},
	
	onClearButtonPress: function () {
		localStorage.clear();
		location.reload();
	}
});