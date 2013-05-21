package org.siemac.metamac.statistical.operations.web.client;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.siemac.metamac.sso.client.MetamacPrincipal;
import org.siemac.metamac.statistical.operations.web.client.gin.OperationsWebGinjector;
import org.siemac.metamac.web.common.client.MetamacEntryPoint;
import org.siemac.metamac.web.common.client.events.LoginAuthenticatedEvent;
import org.siemac.metamac.web.common.client.utils.ApplicationEditionLanguages;
import org.siemac.metamac.web.common.client.utils.ApplicationOrganisation;
import org.siemac.metamac.web.common.client.widgets.MetamacNavBar;
import org.siemac.metamac.web.common.client.widgets.WaitingAsyncCallback;
import org.siemac.metamac.web.common.shared.GetLoginPageUrlAction;
import org.siemac.metamac.web.common.shared.GetLoginPageUrlResult;
import org.siemac.metamac.web.common.shared.GetNavigationBarUrlAction;
import org.siemac.metamac.web.common.shared.GetNavigationBarUrlResult;
import org.siemac.metamac.web.common.shared.LoadConfigurationPropertiesAction;
import org.siemac.metamac.web.common.shared.LoadConfigurationPropertiesResult;
import org.siemac.metamac.web.common.shared.MockCASUserAction;
import org.siemac.metamac.web.common.shared.MockCASUserResult;

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Document;
import com.google.gwt.resources.client.ClientBundle;
import com.google.gwt.resources.client.CssResource;
import com.google.gwt.resources.client.CssResource.NotStrict;
import com.google.gwt.user.client.Window;
import com.gwtplatform.mvp.client.DelayedBindRegistry;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class OperationsWeb extends MetamacEntryPoint {

    private static Logger                      logger    = Logger.getLogger(OperationsWeb.class.getName());

    private static MetamacPrincipal            principal;
    private static OperationsWebConstants      constants;
    private static OperationsWebCoreMessages   coreMessages;
    private static OperationsWebMessages       messages;

    public static final OperationsWebGinjector ginjector = GWT.create(OperationsWebGinjector.class);

    interface GlobalResources extends ClientBundle {

        @NotStrict
        @Source("OperationsWebStyles.css")
        CssResource css();
    }

    @Override
    public void onModuleLoad() {
        setUncaughtExceptionHandler();
        ginjector.getDispatcher().execute(new GetNavigationBarUrlAction(), new WaitingAsyncCallback<GetNavigationBarUrlResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                logger.log(Level.SEVERE, "Error loading toolbar");
                loadNonSecuredApplication();
            }

            @Override
            public void onWaitSuccess(GetNavigationBarUrlResult result) {
                // Load scripts for navigation bar
                if (result.getNavigationBarUrl() != null) {
                    MetamacNavBar.loadScripts(result.getNavigationBarUrl());
                } else {
                    logger.log(Level.SEVERE, "Error loading toolbar");
                }
                loadNonSecuredApplication();
            };
        });

    }

    // TODO This method should be removed to use CAS authentication
    // Application id should be the same than the one defined in org.siemac.metamac.statistical.operations.core.constants.StatisticalOperationsConstants.SECURITY_APPLICATION_ID
    private void loadNonSecuredApplication() {
        ginjector.getDispatcher().execute(new MockCASUserAction("GESTOR_OPERACIONES"), new WaitingAsyncCallback<MockCASUserResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                logger.log(Level.SEVERE, "Error mocking CAS user");
            }
            @Override
            public void onWaitSuccess(MockCASUserResult result) {
                OperationsWeb.principal = result.getMetamacPrincipal();

                // Load edition languages
                ginjector.getDispatcher().execute(new LoadConfigurationPropertiesAction(), new WaitingAsyncCallback<LoadConfigurationPropertiesResult>() {

                    @Override
                    public void onWaitFailure(Throwable caught) {
                        logger.log(Level.SEVERE, "Error loading edition languages");
                        // If an error occurs while loading edition languages, enable SPANISH, ENGLISH and PORTUGUESE by default
                        ApplicationEditionLanguages.setEditionLanguages(new String[]{ApplicationEditionLanguages.SPANISH, ApplicationEditionLanguages.ENGLISH, ApplicationEditionLanguages.PORTUGUESE});
                        loadApplication();
                    }
                    @Override
                    public void onWaitSuccess(LoadConfigurationPropertiesResult result) {
                        ApplicationEditionLanguages.setEditionLanguages(result.getLanguages());
                        ApplicationOrganisation.setCurrentOrganisation(result.getOrganisation());
                        loadApplication();
                    }
                });
            }
        });
    }

    // TODO Restore this method to use CAS authentication
    // private void loadSecuredApplication() {
    // String ticketParam = Window.Location.getParameter(TICKET);
    // if (ticketParam != null) {
    // UrlBuilder urlBuilder = Window.Location.createUrlBuilder();
    // urlBuilder.removeParameter(TICKET);
    // urlBuilder.setHash(Window.Location.getHash() + TICKET_HASH + ticketParam);
    // String url = urlBuilder.buildString();
    // Window.Location.replace(url);
    // return;
    // }
    //
    // String hash = Window.Location.getHash();
    //
    // String ticketHash = null;
    // if (hash.contains(TICKET_HASH)) {
    // ticketHash = hash.substring(hash.indexOf(TICKET_HASH) + TICKET_HASH.length(), hash.length());
    // }
    //
    // if (ticketHash == null || ticketHash.length() == 0) {
    // displayLoginView();
    // } else {
    // String serviceUrl = Window.Location.createUrlBuilder().buildString();
    // ginjector.getDispatcher().execute(new ValidateTicketAction(ticketHash, serviceUrl), new WaitingAsyncCallback<ValidateTicketResult>() {
    //
    // @Override
    // public void onWaitFailure(Throwable arg0) {
    // logger.log(Level.SEVERE, "Error validating ticket");
    // }
    // @Override
    // public void onWaitSuccess(ValidateTicketResult result) {
    // OperationsWeb.principal = result.getMetamacPrincipal();
    //
    // String url = Window.Location.createUrlBuilder().setHash("").buildString();
    // Window.Location.assign(url);
    //
    // // Load edition languages
    // ginjector.getDispatcher().execute(new LoadConfigurationPropertiesAction(), new WaitingAsyncCallback<LoadConfigurationPropertiesResult>() {
    //
    // @Override
    // public void onWaitFailure(Throwable caught) {
    // logger.log(Level.SEVERE, "Error loading edition languages");
    // // If an error occurs while loading edition languages, enable SPANISH, ENGLISH and PORTUGUESE by default
    // ApplicationEditionLanguages.setEditionLanguages(new String[]{ApplicationEditionLanguages.SPANISH, ApplicationEditionLanguages.ENGLISH,
    // ApplicationEditionLanguages.PORTUGUESE});
    // loadApplication();
    // }
    // @Override
    // public void onWaitSuccess(LoadConfigurationPropertiesResult result) {
    // ApplicationEditionLanguages.setEditionLanguages(result.getLanguages());
    // ApplicationOrganisation.setCurrentOrganisation(result.getOrganisation());
    // loadApplication();
    // }
    // });
    // }
    // });
    // }
    // }

    private void loadApplication() {
        LoginAuthenticatedEvent.fire(ginjector.getEventBus(), OperationsWeb.principal);
        // This is required for GWT-Platform proxy's generator.
        DelayedBindRegistry.bind(ginjector);
        ginjector.getPlaceManager().revealCurrentPlace();
        // Inject global styles
        GWT.<GlobalResources> create(GlobalResources.class).css().ensureInjected();
        Document.get().setTitle(getConstants().appTitle());
    }

    public void displayLoginView() {
        String serviceUrl = Window.Location.createUrlBuilder().buildString();
        ginjector.getDispatcher().execute(new GetLoginPageUrlAction(serviceUrl), new WaitingAsyncCallback<GetLoginPageUrlResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                logger.log(Level.SEVERE, "Error getting login page URL");
            }
            @Override
            public void onWaitSuccess(GetLoginPageUrlResult result) {
                Window.Location.replace(result.getLoginPageUrl());
            }
        });
    }

    public static MetamacPrincipal getCurrentUser() {
        return OperationsWeb.principal;
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

    public static void showErrorPage() {
        ginjector.getPlaceManager().revealErrorPlace(null);
    }

}