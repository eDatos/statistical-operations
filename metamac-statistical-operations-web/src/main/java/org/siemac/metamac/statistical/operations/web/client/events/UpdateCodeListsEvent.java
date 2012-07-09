package org.siemac.metamac.statistical.operations.web.client.events;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;

import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HasHandlers;

public class UpdateCodeListsEvent extends GwtEvent<UpdateCodeListsEvent.UpdateCodeListsHandler> {

    public interface UpdateCodeListsHandler extends EventHandler {

        void onUpdateCodeLists(UpdateCodeListsEvent event);
    }

    private static Type<UpdateCodeListsHandler> TYPE = new Type<UpdateCodeListsHandler>();

    @Override
    public com.google.gwt.event.shared.GwtEvent.Type<UpdateCodeListsHandler> getAssociatedType() {
        return TYPE;
    }

    // TODO HasEventBus should be used instead of HasHandlers ¿?
    public static void fire(HasHandlers source, List<ExternalItemDto> codeLists) {
        if (TYPE != null) {
            source.fireEvent(new UpdateCodeListsEvent(codeLists));
        }
    }

    @Override
    protected void dispatch(UpdateCodeListsHandler handler) {
        handler.onUpdateCodeLists(this);
    }

    private final List<ExternalItemDto> codeLists;

    public UpdateCodeListsEvent(List<ExternalItemDto> codeLists) {
        this.codeLists = codeLists;
    }

    public List<ExternalItemDto> getCodeLists() {
        return codeLists;
    }

    public static Type<UpdateCodeListsHandler> getType() {
        return TYPE;
    }

}
