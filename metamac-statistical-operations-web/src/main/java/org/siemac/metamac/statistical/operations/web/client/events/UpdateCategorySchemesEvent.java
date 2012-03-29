package org.siemac.metamac.gopestat.web.client.events;

import java.util.List;

import org.siemac.metamac.core.common.dto.serviceapi.ExternalItemBtDto;

import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HasHandlers;

public class UpdateCategorySchemesEvent extends GwtEvent<UpdateCategorySchemesEvent.UpdateCategorySchemesHandler> {

    public interface UpdateCategorySchemesHandler extends EventHandler {

        void onUpdateCategorySchemes(UpdateCategorySchemesEvent event);
    }

    private static Type<UpdateCategorySchemesHandler> TYPE = new Type<UpdateCategorySchemesHandler>();

    @Override
    public com.google.gwt.event.shared.GwtEvent.Type<UpdateCategorySchemesHandler> getAssociatedType() {
        return TYPE;
    }

    // TODO HasEventBus should be used instead of HasHandlers ¿?
    public static void fire(HasHandlers source, List<ExternalItemBtDto> categorySchemes) {
        if (TYPE != null) {
            source.fireEvent(new UpdateCategorySchemesEvent(categorySchemes));
        }
    }

    @Override
    protected void dispatch(UpdateCategorySchemesHandler handler) {
        handler.onUpdateCategorySchemes(this);
    }

    private final List<ExternalItemBtDto> categorySchemes;

    public UpdateCategorySchemesEvent(List<ExternalItemBtDto> categorySchemes) {
        this.categorySchemes = categorySchemes;
    }

    public List<ExternalItemBtDto> getCategorySchemes() {
        return categorySchemes;
    }

    public static Type<UpdateCategorySchemesHandler> getType() {
        return TYPE;
    }

}
