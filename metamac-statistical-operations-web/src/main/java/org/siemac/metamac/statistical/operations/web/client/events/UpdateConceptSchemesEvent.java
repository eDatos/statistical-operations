package org.siemac.metamac.statistical.operations.web.client.events;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;

import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HasHandlers;

public class UpdateConceptSchemesEvent extends GwtEvent<UpdateConceptSchemesEvent.UpdateConceptSchemesHandler> {

    public interface UpdateConceptSchemesHandler extends EventHandler {

        void onUpdateConceptSchemes(UpdateConceptSchemesEvent event);
    }

    private static Type<UpdateConceptSchemesHandler> TYPE = new Type<UpdateConceptSchemesHandler>();

    @Override
    public com.google.gwt.event.shared.GwtEvent.Type<UpdateConceptSchemesHandler> getAssociatedType() {
        return TYPE;
    }

    // TODO HasEventBus should be used instead of HasHandlers ¿?
    public static void fire(HasHandlers source, List<ExternalItemDto> conceptSchemes) {
        if (TYPE != null) {
            source.fireEvent(new UpdateConceptSchemesEvent(conceptSchemes));
        }
    }

    @Override
    protected void dispatch(UpdateConceptSchemesHandler handler) {
        handler.onUpdateConceptSchemes(this);
    }

    private final List<ExternalItemDto> conceptSchemes;

    public UpdateConceptSchemesEvent(List<ExternalItemDto> conceptSchemes) {
        this.conceptSchemes = conceptSchemes;
    }

    public List<ExternalItemDto> getConceptSchemes() {
        return conceptSchemes;
    }

    public static Type<UpdateConceptSchemesHandler> getType() {
        return TYPE;
    }

}
