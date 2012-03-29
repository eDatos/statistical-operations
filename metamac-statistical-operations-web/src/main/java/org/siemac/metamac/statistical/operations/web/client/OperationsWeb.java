package org.siemac.metamac.statistical.operations.web.client;

import org.siemac.metamac.statistical.operations.web.client.gin.OperationsWebGinjector;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.resources.client.ClientBundle;
import com.google.gwt.resources.client.CssResource;
import com.google.gwt.resources.client.CssResource.NotStrict;
import com.gwtplatform.mvp.client.DelayedBindRegistry;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class OperationsWeb implements EntryPoint {

    interface GlobalResources extends ClientBundle {

        @NotStrict
        @Source("OperationsWebStyles.css")
        CssResource css();
    }

    private static OperationsWebConstants      constants;
    private static OperationsWebCoreMessages   coreMessages;
    private static OperationsWebMessages       messages;

    public static final OperationsWebGinjector ginjector = GWT.create(OperationsWebGinjector.class);

    public void onModuleLoad() {
        // This is required for GWT-Platform proxy's generator.
        DelayedBindRegistry.bind(ginjector);
        ginjector.getPlaceManager().revealCurrentPlace();

        // Inject global styles
        GWT.<GlobalResources> create(GlobalResources.class).css().ensureInjected();
    }

    public static OperationsWebConstants getConstants() {
        if (constants == null) {
            constants = (OperationsWebConstants) GWT.create(OperationsWebConstants.class);
        }
        return constants;
    }

    public static OperationsWebCoreMessages getCoreMessages() {
        if (coreMessages == null) {
            coreMessages = (OperationsWebCoreMessages) GWT.create(OperationsWebCoreMessages.class);
        }
        return coreMessages;
    }

    public static OperationsWebMessages getMessages() {
        if (messages == null) {
            messages = (OperationsWebMessages) GWT.create(OperationsWebMessages.class);
        }
        return messages;
    }

    public static OperationsWebGinjector getOperationsWebGinjector() {
        return ginjector;
    }

}