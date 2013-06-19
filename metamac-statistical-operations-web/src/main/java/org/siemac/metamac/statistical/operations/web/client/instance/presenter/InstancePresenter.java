package org.siemac.metamac.statistical.operations.web.client.instance.presenter;

import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getConstants;
import static org.siemac.metamac.statistical.operations.web.client.OperationsWeb.getMessages;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.constants.shared.UrnConstants;
import org.siemac.metamac.core.common.enume.domain.TypeExternalArtefactsEnum;
import org.siemac.metamac.core.common.util.shared.StringUtils;
import org.siemac.metamac.core.common.util.shared.UrnUtils;
import org.siemac.metamac.statistical.operations.core.dto.CollMethodDto;
import org.siemac.metamac.statistical.operations.core.dto.CostDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceTypeDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.SurveySourceDto;
import org.siemac.metamac.statistical.operations.navigation.shared.NameTokens;
import org.siemac.metamac.statistical.operations.navigation.shared.PlaceRequestParams;
import org.siemac.metamac.statistical.operations.web.client.LoggedInGatekeeper;
import org.siemac.metamac.statistical.operations.web.client.OperationsWeb;
import org.siemac.metamac.statistical.operations.web.client.enums.ToolStripButtonEnum;
import org.siemac.metamac.statistical.operations.web.client.events.SelectMenuButtonEvent;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateOperationsListsEvent;
import org.siemac.metamac.statistical.operations.web.client.events.UpdateOperationsListsEvent.UpdateOperationsListsHandler;
import org.siemac.metamac.statistical.operations.web.client.instance.view.handlers.InstanceUiHandlers;
import org.siemac.metamac.statistical.operations.web.client.presenter.MainPagePresenter;
import org.siemac.metamac.statistical.operations.web.client.utils.PlaceRequestUtils;
import org.siemac.metamac.statistical.operations.web.shared.DeleteInstanceListAction;
import org.siemac.metamac.statistical.operations.web.shared.DeleteInstanceListResult;
import org.siemac.metamac.statistical.operations.web.shared.GetInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.GetInstanceResult;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyInstanceResult;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyInstanceResult;
import org.siemac.metamac.statistical.operations.web.shared.SaveInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveInstanceResult;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.ConceptSchemeRestCriteria;
import org.siemac.metamac.statistical.operations.web.shared.external.GetExternalResourcesAction;
import org.siemac.metamac.statistical.operations.web.shared.external.GetExternalResourcesResult;
import org.siemac.metamac.statistical.operations.web.shared.external.RestWebCriteriaUtils;
import org.siemac.metamac.web.common.client.events.ShowMessageEvent;
import org.siemac.metamac.web.common.client.events.UpdatePlaceHistoryEvent;
import org.siemac.metamac.web.common.client.widgets.WaitingAsyncCallback;
import org.siemac.metamac.web.common.shared.criteria.ExternalResourceWebCriteria;
import org.siemac.metamac.web.common.shared.criteria.SrmItemRestCriteria;
import org.siemac.metamac.web.common.shared.domain.ExternalItemsResult;

import com.google.gwt.event.shared.EventBus;
import com.google.inject.Inject;
import com.gwtplatform.dispatch.shared.DispatchAsync;
import com.gwtplatform.mvp.client.HasUiHandlers;
import com.gwtplatform.mvp.client.Presenter;
import com.gwtplatform.mvp.client.View;
import com.gwtplatform.mvp.client.annotations.NameToken;
import com.gwtplatform.mvp.client.annotations.ProxyCodeSplit;
import com.gwtplatform.mvp.client.annotations.ProxyEvent;
import com.gwtplatform.mvp.client.annotations.TitleFunction;
import com.gwtplatform.mvp.client.annotations.UseGatekeeper;
import com.gwtplatform.mvp.client.proxy.Place;
import com.gwtplatform.mvp.client.proxy.PlaceManager;
import com.gwtplatform.mvp.client.proxy.PlaceRequest;
import com.gwtplatform.mvp.client.proxy.Proxy;
import com.gwtplatform.mvp.client.proxy.RevealContentEvent;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.HasClickHandlers;

public class InstancePresenter extends Presenter<InstancePresenter.InstanceView, InstancePresenter.InstanceProxy> implements InstanceUiHandlers, UpdateOperationsListsHandler {

    private final DispatchAsync dispatcher;
    private final PlaceManager  placeManager;

    private OperationBaseDto    operationBaseDto;
    private InstanceDto         instanceDto;

    public static final Object  TYPE_SetContextAreaContentToolBar = new Object();

    @ProxyCodeSplit
    @NameToken(NameTokens.instancePage)
    @UseGatekeeper(LoggedInGatekeeper.class)
    public interface InstanceProxy extends Proxy<InstancePresenter>, Place {

    }

    @TitleFunction
    public static String getTranslatedTitle(PlaceRequest placeRequest) {
        String instanceCode = placeRequest.getParameter(PlaceRequestParams.instanceParam, null);
        return instanceCode != null ? instanceCode : getConstants().breadcrumbInstance();
    }

    public interface InstanceView extends View, HasUiHandlers<InstanceUiHandlers> {

        void setInstance(InstanceDto instanceDto, String operationCode);
        HasClickHandlers getSave();
        void onInstanceSaved(InstanceDto instanceDto);
        InstanceDto getInstance(InstanceDto instanceDto);

        void setOperationsLists(List<InstanceTypeDto> instanceTypeDtos, List<SurveySourceDto> surveySourceDtos, List<CollMethodDto> collMethodDtos, List<CostDto> costDtos);

        // External resources

        void setItemSchemes(String formItemName, ExternalItemsResult result);
        void setItems(String formItemName, ExternalItemsResult result);

        boolean validate();
        HasClickHandlers getPublishInstanceInternally();
        HasClickHandlers getPublishInstanceExternally();
    }

    @Inject
    public InstancePresenter(EventBus eventBus, InstanceView instanceView, InstanceProxy instanceProxy, DispatchAsync dispatcher, PlaceManager placeManager) {
        super(eventBus, instanceView, instanceProxy);
        this.dispatcher = dispatcher;
        this.placeManager = placeManager;
        getView().setUiHandlers(this);
    }

    @Override
    protected void revealInParent() {
        RevealContentEvent.fire(this, MainPagePresenter.TYPE_SetContextAreaContent, this);
    }

    @Override
    protected void onBind() {
        super.onBind();

        registerHandler(getView().getSave().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                if (getView().validate()) {
                    saveInstance(getView().getInstance(instanceDto));
                }
            }
        }));

        registerHandler(getView().getPublishInstanceInternally().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                publishInstanceInternally();
            }
        }));

        registerHandler(getView().getPublishInstanceExternally().addClickHandler(new ClickHandler() {

            @Override
            public void onClick(ClickEvent event) {
                publishInstanceExternally();
            }
        }));
    }

    @Override
    protected void onReveal() {
        super.onReveal();
        MainPagePresenter.getMasterHead().setTitleLabel(getConstants().instance());
        SelectMenuButtonEvent.fire(this, ToolStripButtonEnum.OPERATIONS);
    }

    @Override
    public void prepareFromRequest(PlaceRequest request) {
        super.prepareFromRequest(request);
        String operationCode = PlaceRequestUtils.getOperationIdentifierParam(placeManager);
        String instanceCode = request.getParameter(PlaceRequestParams.instanceParam, null);
        if (!StringUtils.isBlank(operationCode) && !StringUtils.isBlank(instanceCode)) {
            retrieveInstance(operationCode, instanceCode);
        } else {
            OperationsWeb.showErrorPage();
        }
    }

    @Override
    public void saveInstance(InstanceDto instanceToSave) {
        dispatcher.execute(new SaveInstanceAction(operationBaseDto.getId(), instanceToSave), new WaitingAsyncCallback<SaveInstanceResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fireErrorMessage(InstancePresenter.this, caught);
            }
            @Override
            public void onWaitSuccess(SaveInstanceResult result) {
                ShowMessageEvent.fireSuccessMessage(InstancePresenter.this, getMessages().instanceSaved());
                instanceDto = result.getInstanceSaved();
                getView().onInstanceSaved(instanceDto);

                // Update URL
                PlaceRequest placeRequest = PlaceRequestUtils.buildRelativeInstancePlaceRequest(instanceDto.getCode());
                // placeManager.updateHistory(placeRequest, true);
                UpdatePlaceHistoryEvent.fireUpdate(InstancePresenter.this, placeRequest);
                MainPagePresenter.getMasterHead().setTitleLabel(getMessages().titleInstance(instanceDto.getCode()));

            }
        });
    }

    @Override
    public void deleteInstance(InstanceDto instanceDto) {
        final List<Long> ids = new ArrayList<Long>();
        ids.add(instanceDto.getId());
        dispatcher.execute(new DeleteInstanceListAction(ids), new WaitingAsyncCallback<DeleteInstanceListResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fireErrorMessage(InstancePresenter.this, caught);
            }
            @Override
            public void onWaitSuccess(DeleteInstanceListResult result) {
                placeManager.revealRelativePlace(-1);
            }
        });
    }

    private void retrieveInstance(String operationCode, String instanceCode) {
        String instanceUrn = UrnUtils.generateUrn(UrnConstants.URN_SIEMAC_CLASS_INSTANCE_PREFIX, operationCode, instanceCode);
        dispatcher.execute(new GetInstanceAction(instanceUrn), new WaitingAsyncCallback<GetInstanceResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                OperationsWeb.showErrorPage();
                ShowMessageEvent.fireErrorMessage(InstancePresenter.this, caught);
            }
            @Override
            public void onWaitSuccess(GetInstanceResult result) {
                instanceDto = result.getInstanceDto();
                operationBaseDto = result.getOperationBaseDto();
                MainPagePresenter.getMasterHead().setTitleLabel(getMessages().titleInstance(instanceDto.getCode()));
                getView().setInstance(instanceDto, result.getOperationBaseDto().getCode());
            }
        });
    }

    private void publishInstanceInternally() {
        dispatcher.execute(new PublishInternallyInstanceAction(instanceDto.getId()), new WaitingAsyncCallback<PublishInternallyInstanceResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fireErrorMessage(InstancePresenter.this, caught);
            }
            @Override
            public void onWaitSuccess(PublishInternallyInstanceResult result) {
                instanceDto = result.getInstanceSaved();
                getView().onInstanceSaved(instanceDto);
                ShowMessageEvent.fireSuccessMessage(InstancePresenter.this, getMessages().instanceInternallyPublished());
            }
        });
    }

    private void publishInstanceExternally() {
        dispatcher.execute(new PublishExternallyInstanceAction(instanceDto.getId()), new WaitingAsyncCallback<PublishExternallyInstanceResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fireErrorMessage(InstancePresenter.this, caught);
            }
            @Override
            public void onWaitSuccess(PublishExternallyInstanceResult result) {
                instanceDto = result.getInstanceSaved();
                getView().onInstanceSaved(instanceDto);
                ShowMessageEvent.fireSuccessMessage(InstancePresenter.this, getMessages().instanceExternallyPublished());
            }
        });
    }

    @ProxyEvent
    @Override
    public void onUpdateOperationsLists(UpdateOperationsListsEvent event) {
        getView().setOperationsLists(event.getInstanceTypeDtos(), event.getSurveySourceDtos(), event.getCollMethodDtos(), event.getCostDtos());
    }

    //
    // EXTERNAL RESOURCES
    //

    @Override
    public void retrieveItemSchemes(final String formItemName, TypeExternalArtefactsEnum[] types, int firstResult, int maxResults, String criteria) {
        ExternalResourceWebCriteria externalResourceWebCriteria = RestWebCriteriaUtils.buildItemSchemeWebCriteria(types, criteria);
        retrieveItemSchemes(formItemName, externalResourceWebCriteria, firstResult, maxResults);
    }

    @Override
    public void retrieveItemSchemes(final String formItemName, ExternalResourceWebCriteria externalResourceWebCriteria, int firstResult, int maxResults) {
        if (externalResourceWebCriteria instanceof ConceptSchemeRestCriteria) {
            ((ConceptSchemeRestCriteria) externalResourceWebCriteria).setStatisticalOperationUrn(operationBaseDto.getUrn());
        }
        dispatcher.execute(new GetExternalResourcesAction(externalResourceWebCriteria, firstResult, maxResults), new WaitingAsyncCallback<GetExternalResourcesResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fireErrorMessage(InstancePresenter.this, caught);
            }
            @Override
            public void onWaitSuccess(GetExternalResourcesResult result) {
                getView().setItemSchemes(formItemName, result.getExternalItemsResult());
            }
        });
    }
    @Override
    public void retrieveItems(final String formItemName, TypeExternalArtefactsEnum[] types, int firstResult, int maxResults, String criteria, String itemSchemeUrn) {
        SrmItemRestCriteria itemWebCriteria = RestWebCriteriaUtils.buildItemWebCriteria(types, criteria, itemSchemeUrn);
        retrieveItems(formItemName, itemWebCriteria, firstResult, maxResults);
    }

    @Override
    public void retrieveItems(final String formItemName, SrmItemRestCriteria itemWebCriteria, int firstResult, int maxResults) {
        if (itemWebCriteria instanceof ConceptRestCriteria) {
            ((ConceptRestCriteria) itemWebCriteria).setStatisticalOperationUrn(operationBaseDto.getUrn());
        }
        dispatcher.execute(new GetExternalResourcesAction(itemWebCriteria, firstResult, maxResults), new WaitingAsyncCallback<GetExternalResourcesResult>() {

            @Override
            public void onWaitFailure(Throwable caught) {
                ShowMessageEvent.fireErrorMessage(InstancePresenter.this, caught);
            }
            @Override
            public void onWaitSuccess(GetExternalResourcesResult result) {
                getView().setItems(formItemName, result.getExternalItemsResult());
            }
        });
    }

    //
    // NAVIGATION
    //

    @Override
    public void goTo(List<PlaceRequest> location) {
        if (location != null && !location.isEmpty()) {
            placeManager.revealPlaceHierarchy(location);
        }
    }
}
