package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.statistical.operations.web.client.operation.view.handlers.OperationUiHandlers;
import org.siemac.metamac.web.common.client.MetamacWebCommon;
import org.siemac.metamac.web.common.client.model.ds.RelatedResourceBaseDS;
import org.siemac.metamac.web.common.client.model.record.ExternalItemRecord;
import org.siemac.metamac.web.common.client.utils.RecordUtils;
import org.siemac.metamac.web.common.client.widgets.CustomListGridField;
import org.siemac.metamac.web.common.client.widgets.SearchWindow;
import org.siemac.metamac.web.common.client.widgets.form.fields.SearchExternalItemLinkItem;

import com.smartgwt.client.types.Autofit;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemClickHandler;
import com.smartgwt.client.widgets.form.fields.events.FormItemIconClickEvent;

public class SearchCommonMetadataItem extends SearchExternalItemLinkItem {

    private SearchExternalItemsWindow searchWindow;
    private OperationUiHandlers       uiHandlers;
    private ClickHandler              saveClickHandler;

    public SearchCommonMetadataItem(String name, String title) {
        super(name, title);

        getSearchIcon().addFormItemClickHandler(new FormItemClickHandler() {

            @Override
            public void onFormItemClick(FormItemIconClickEvent event) {
                searchWindow = new SearchExternalItemsWindow(getConstants().searchCommonMetadataConfigurations());
                searchWindow.getAcceptButton().addClickHandler(saveClickHandler);
                getUiHandlers().retrieveCommonMetadataConfigurations();
            }
        });
    }

    public void setCommonMetadataConfigurations(List<ExternalItemDto> commonMetadataList) {
        if (searchWindow != null) {
            searchWindow.getListGrid().setData(RecordUtils.getExternalItemRecords(commonMetadataList));
        }
    }

    public OperationUiHandlers getUiHandlers() {
        return uiHandlers;
    }

    public void setUiHandlers(OperationUiHandlers uiHandlers) {
        this.uiHandlers = uiHandlers;
    }

    public void setSaveClickHandler(ClickHandler clickHandler) {
        this.saveClickHandler = clickHandler;
    }

    public ExternalItemDto getSelectedExternalItemDto() {
        ExternalItemRecord record = (ExternalItemRecord) searchWindow.getListGrid().getSelectedRecord();
        return record.getExternalItemBtDto();
    }

    public void markSearchWindowForDestroy() {
        searchWindow.markForDestroy();
    }

    private class SearchExternalItemsWindow extends SearchWindow {

        public SearchExternalItemsWindow(String title) {
            super(title);

            setHeight(200);
            setAutoSize(true);

            CustomListGridField codeField = new CustomListGridField(RelatedResourceBaseDS.CODE, MetamacWebCommon.getConstants().externalItemCode());
            CustomListGridField titleField = new CustomListGridField(RelatedResourceBaseDS.TITLE, MetamacWebCommon.getConstants().externalItemTitle());
            CustomListGridField urnField = new CustomListGridField(RelatedResourceBaseDS.URN, MetamacWebCommon.getConstants().externalItemURN());

            listGrid.setAutoFitMaxRecords(6);
            listGrid.setAutoFitData(Autofit.VERTICAL);
            listGrid.setFields(codeField, titleField, urnField);
            listGrid.setMargin(15);

            show();
        }
    }
}
