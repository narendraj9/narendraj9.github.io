sap.ui.controller("cart.ProductsList", {
	onSelectionChange : function(_, _, _, oSelectedItem) {
		// Get the selected item
		var oSelectionList = this.getView().byId(this.createId("selectList"));
		var data = oSelectionList.getSelectedItem().mProperties;
		var fullData = {};

		// Slow but would work
		var rootModel = sap.ui.getCore().byId("idMain1").getModel();
		var pList = rootModel.getData().ProductCollection;

		console.log(pList);

		console.log(pList[0]);
		for ( var p in pList) {
			if (pList[p].ProductId == data.key) {
				fullData = pList[p];
				console.log("Found the data item.");
				break;
			}
		}

		console.log(fullData);

		// Modify details page model
		var mainPage = sap.ui.getCore().byId("idMain1");
		var detailsView = mainPage.byId("detailsView");
		detailsView.getModel().setData(fullData);

		// Update the last visited product in the select list.
		var newCurrentCart = JSON.parse(localStorage.getItem('currentCart'));
		newCurrentCart.lastProduct = fullData;
		localStorage.setItem('currentCart', JSON.stringify(newCurrentCart));
	}
});
