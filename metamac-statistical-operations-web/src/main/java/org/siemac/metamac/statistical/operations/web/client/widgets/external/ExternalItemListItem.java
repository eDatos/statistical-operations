package org.siemac.metamac.statistical.operations.web.client.widgets.external;

import java.util.List;
import java.util.Set;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.util.shared.StringUtils;
import org.siemac.metamac.web.common.client.model.record.ExternalItemRecord;
import org.siemac.metamac.web.common.client.utils.NavigationUtils;
import org.siemac.metamac.web.common.client.utils.RecordUtils;
import org.siemac.metamac.web.common.client.widgets.form.fields.RelatedResourceBaseListItem;

import com.gwtplatform.mvp.client.proxy.PlaceRequest;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.widgets.grid.events.RecordClickEvent;
import com.smartgwt.client.widgets.grid.events.RecordClickHandler;

public class ExternalItemListItem extends RelatedResourceBaseListItem<ExternalItemDto> {

    public ExternalItemListItem(String name, String title, boolean editionMode) {
        super(name, title, editionMode);
        recordClickHandlerRegistration = listGrid.addRecordClickHandler(new RecordClickHandler() {

            @Override
            public void onRecordClick(RecordClickEvent event) {
                Record record = event.getRecord();
                if (record != null && record instanceof ExternalItemRecord) {
                    String url = ((ExternalItemRecord) record).getManagementAppUrl();
                    if (!StringUtils.isBlank(url)) {
                        NavigationUtils.goTo(url);
                    }
                }
            }
        });
    }

    public void setExternalItems(List<ExternalItemDto> externalItemDtos) {
        listGrid.removeAllData();
        ExternalItemRecord[] records = RecordUtils.getExternalItemRecords(externalItemDtos);
        listGrid.setData(records);
    }

    public List<ExternalItemDto> getSelectedExternalItemDtos() {
        return getSelectedRelatedResources();
    }

    @Override
    protected List<PlaceRequest> buildLocation(ExternalItemDto externalItemDto) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setRelatedResources(List<ExternalItemDto> relatedResourceDtos) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setRelatedResources(Set<ExternalItemDto> relatedResourceDtos) {
        throw new UnsupportedOperationException();
    }
}
