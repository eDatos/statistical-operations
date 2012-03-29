package org.siemac.metamac.statistical.operations.web.client.events;

import java.util.List;

import org.siemac.metamac.core.common.dto.serviceapi.ExternalItemBtDto;

import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HasHandlers;

public class UpdateCommonMetadataEvent extends GwtEvent<UpdateCommonMetadataEvent.UpdateCommonMetadataHandler> {

    public interface UpdateCommonMetadataHandler extends EventHandler {

        void onUpdateCommonMetadata(UpdateCommonMetadataEvent event);
    }

    private static Type<UpdateCommonMetadataHandler> TYPE = new Type<UpdateCommonMetadataHandler>();

    @Override
    public com.google.gwt.event.shared.GwtEvent.Type<UpdateCommonMetadataHandler> getAssociatedType() {
        return TYPE;
    }

    // TODO HasEventBus should be used instead of HasHandlers ¿?
    public static void fire(HasHandlers source, List<ExternalItemBtDto> categorySchemes) {
        if (TYPE != null) {
            source.fireEvent(new UpdateCommonMetadataEvent(categorySchemes));
        }
    }

    @Override
    protected void dispatch(UpdateCommonMetadataHandler handler) {
        handler.onUpdateCommonMetadata(this);
    }

    private final List<ExternalItemBtDto> commonMetadataList;

    public UpdateCommonMetadataEvent(List<ExternalItemBtDto> commonMetadataList) {
        this.commonMetadataList = commonMetadataList;
    }

    public List<ExternalItemBtDto> getCommonMetadata() {
        return commonMetadataList;
    }

    public static Type<UpdateCommonMetadataHandler> getType() {
        return TYPE;
    }

}
