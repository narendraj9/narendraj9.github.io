sap.ui.controller("cart.CurrentCart", {
	onInit : function() {
		var currentCart = JSON.parse(localStorage.getItem('currentCart'));
		console.log(currentCart);
		var oModel = new sap.ui.model.json.JSONModel(currentCart);
		this.getView().setModel(oModel);
		console.log("Model set on current cart view");
	},

	onHomeButtonPress : function() {
		// First figure out how to navigate
		location.reload();
	},

	onClearButtonPress : function() {

		var currentCart = JSON.parse(localStorage.getItem("currentCart"));
		currentCart.cart = [];
		localStorage.setItem("currentCart", JSON.stringify(currentCart));

		this.getView().getModel().setData({
			cart : [],
			totalAmount : 0
		});

		sap.m.MessageToast.show("Cleared the Cart", {
			duration : 3000,
			at : "Center Center"
		});
	},

	onSendApprovalPress : function() {

		sap.m.MessageToast.show("Sending for approval", {
			duration : 3000,
			at : sap.ui.core.Popup.Dock.CenterCenter
		});

		var currentCart = JSON.parse(localStorage.getItem("currentCart"));

		// Assuming I am not very unlucky to get collisions here. It doesn't
		// matter anyway.
		var oData = this.getView().getModel().getData();
		var id = Math.random().toString(36).slice(2);
		var status = 'Awaiting Approval';
		var length = oData.cart.length;
		console.log(oData);
		var totalAmount = oData.totalAmount;
		
		if (!(length > 0)) {
			sap.m.MessageToast.show("The Cart is Empty now. Please add items.",
					{
						duration : 3000,
						at : sap.ui.core.Popup.Dock.CenterCenter
					});
			return;
		}
		var oNewHistoryEntry = {
			oData : oData,
			count : oData.cart.length,
			id : id,
			status : status,
			totalAmount: totalAmount
		};
		
		console.log("Adding a new cart history item:");
		console.log(oNewHistoryEntry);
		
		currentCart.history.push(oNewHistoryEntry);

		localStorage.setItem("currentCart", JSON.stringify(currentCart));

		sap.m.MessageToast.show("Sent for Approval", {
			duration : 3000,
			at : sap.ui.core.Popup.Dock.CenterCenter
		});

	},

	onCartHistoryPress : function() {
		var historyView = new sap.ui.xmlview({
			viewName : "cart.CartHistory"
		});

		var shell = sap.ui.getCore().byId("mainShell");
		shell.setApp(historyView);
	}
});