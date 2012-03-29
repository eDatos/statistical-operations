package org.siemac.metamac.gopestat.web.client;

import org.siemac.metamac.gopestat.web.client.gin.GopestatWebGinjector;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.resources.client.ClientBundle;
import com.google.gwt.resources.client.CssResource;
import com.google.gwt.resources.client.CssResource.NotStrict;
import com.gwtplatform.mvp.client.DelayedBindRegistry;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class GopestatWeb implements EntryPoint {

    interface GlobalResources extends ClientBundle {

        @NotStrict
        @Source("GopestatWebStyles.css")
        CssResource css();
    }

    private static GopestatWebConstants      constants;
    private static GopestatWebCoreMessages   coreMessages;
    private static GopestatWebMessages       messages;

    public static final GopestatWebGinjector ginjector = GWT.create(GopestatWebGinjector.class);

    public void onModuleLoad() {
        // This is required for GWT-Platform proxy's generator.
        DelayedBindRegistry.bind(ginjector);
        ginjector.getPlaceManager().revealCurrentPlace();

        // Inject global styles
        GWT.<GlobalResources> create(GlobalResources.class).css().ensureInjected();
    }

    public static GopestatWebConstants getConstants() {
        if (constants == null) {
            constants = (GopestatWebConstants) GWT.create(GopestatWebConstants.class);
        }
        return constants;
    }

    public static GopestatWebCoreMessages getCoreMessages() {
        if (coreMessages == null) {
            coreMessages = (GopestatWebCoreMessages) GWT.create(GopestatWebCoreMessages.class);
        }
        return coreMessages;
    }

    public static GopestatWebMessages getMessages() {
        if (messages == null) {
            messages = (GopestatWebMessages) GWT.create(GopestatWebMessages.class);
        }
        return messages;
    }

    public static GopestatWebGinjector getGopestatWebGinjector() {
        return ginjector;
    }

}